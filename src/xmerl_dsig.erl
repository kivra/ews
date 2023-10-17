%% -*- coding: utf-8 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2022-2023 Kivra
%%%
%%% Distribution subject to the terms of the LGPL-3.0-or-later, see
%%% the COPYING.LESSER file in the root of the distribution
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% esaml - SAML for erlang
%%
%% Copyright (c) 2013, Alex Wilson and the University of Queensland
%% All rights reserved.
%%
%% Distributed subject to the terms of the 2-clause BSD license, see
%% the BSD2 file in the root of the distribution.

%% @doc XML digital signatures for xmerl
%%
%% Functions for performing XML digital signature generation and
%% verification, as specified at http://www.w3.org/TR/xmldsig-core/ .
%%
%% These routines work on xmerl data structures (see the xmerl user guide
%% for details).
%%
%% Currently only RSA + SHA1|SHA256 signatures are supported, in the typical
%% enveloped mode.
-module(xmerl_dsig).

-export([verify/1, verify/2, sign/3]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("public_key/include/public_key.hrl").

-type xml_thing() :: #xmlDocument{} | #xmlElement{} | #xmlAttribute{} | #xmlPI{} | #xmlText{} | #xmlComment{}.
-type fingerprint() :: binary() | {sha | sha256, binary()}.

%% @doc Signs the given XML element by creating a ds:Signature element within it, returning
%%      the element with the signature added.
%%
%% Don't use "ds" as a namespace prefix in the envelope document, or things will go baaaad.
%% This function has now been modified to do Skatteverket's funky removing of
%% all namespace prefixes before doing a digest. Also there is function
%% that recreates bugs, which can be removed if you are not implementing
%% the Skatteverket API.
%% The private key is now hidden inside a fun so it isn't visible in crashes.
%% It will now sign all signatures inside the SOAP structure.
-spec sign(Element :: #xmlElement{},
           PrivateKeyFun :: fun(() -> #'RSAPrivateKey'{}),
           CertBin :: binary()) -> #xmlElement{}.
sign(ElementIn, PrivateKeyFun, CertBin) when
      is_binary(CertBin) ->
    case xmerl_xpath:string(
           "//*[\"Signature\"=local-name() and \"http://www.w3.org/2000/09/"
           "xmldsig#\"=namespace-uri()]", ElementIn) of
        [] ->
            {error, no_signature};
        Signatures when is_list(Signatures) ->
            sign_signatures(lists:reverse(Signatures), ElementIn, PrivateKeyFun,
                            CertBin)
    end.

sign_signatures([Signature|Tail], ElementIn, PrivateKeyFun, CertBin) ->
    PrivateKey = #'RSAPrivateKey'{} = PrivateKeyFun(),
    logger:debug("Signature: ~p~n", [Signature]),
    TransformAlgo =
        xmerl_xpath:string(
          "//*[\"Transform\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]/@Algorithm", Signature),
    [#xmlAttribute{value = SignatureMethodAlgorithm}] =
        xmerl_xpath:string(
          "//*[\"SignatureMethod\"=local-name() and \"http://www.w3.org/2000/"
          "09/xmldsig#\"=namespace-uri()]/@Algorithm", Signature),
    {HashFunction, _DigestMethod, SignatureMethodAlgorithm} =
        signature_props(SignatureMethodAlgorithm),

    [#xmlAttribute{value = "http://www.w3.org/2001/10/xml-exc-c14n#"}] =
        xmerl_xpath:string(
          "//*[\"CanonicalizationMethod\"=local-name() and \"http://www.w3.org/"
          "2000/09/xmldsig#\"=namespace-uri()]/@Algorithm", Signature),
    [#xmlAttribute{value = SignatureMethodAlgorithm}] =
        xmerl_xpath:string(
          "//*[\"SignatureMethod\"=local-name() and \"http://www.w3.org/2000/"
          "09/xmldsig#\"=namespace-uri()]/@Algorithm", Signature),
    [_C14nTx = #xmlElement{}] =
        xmerl_xpath:string(
          "//*[\"Transform\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]", Signature),
         %% xmerl_xpath:string(
         %%  "ds:Signature/ds:SignedInfo/ds:Reference/ds:Transforms/ds:Transform"
         %%  "[@Algorithm='http://www.w3.org/2001/10/xml-exc-c14n#']",
         %%  Signature, [{namespace, DsNs}]),
    TransformAlgo =
        xmerl_xpath:string(
          "//*[\"Transform\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]/@Algorithm", Signature),


    %% first we need the digest, to generate our SignedInfo element
    [#xmlAttribute{value = RefUri}] =
        xmerl_xpath:string(
          "//*[\"Reference\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]/@URI", Signature),
    logger:debug("Ref: ~p~n", [RefUri]),
    StrippedXml = strip_it(RefUri, TransformAlgo, Signature, ElementIn),
    CanonishXml = xmerl:export_simple([StrippedXml], xmerl_xml),
    <<"<?xml version=\"1.0\"?>", CanonXmlUtf8/binary>> =
        unicode:characters_to_binary(CanonishXml, unicode, utf8),
    logger:debug("DigestInput:~n~ts~n --~n",
                  [CanonXmlUtf8]),
    DigestValue = crypto:hash(HashFunction, CanonXmlUtf8),
    DigestB64 = base64:encode(DigestValue),
    logger:debug("DigestValue: ~s~nDigestB64:~p~n",
                  [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= DigestValue ],
                   DigestB64]),

    NameSpaces = Signature#xmlElement.namespace#xmlNamespace.nodes,
    logger:debug("NameSpaces: ~p", [NameSpaces]),
    [DsPrefix] = [ P || {P, 'http://www.w3.org/2000/09/xmldsig#'} <- NameSpaces ],
    logger:debug("DsPrefix: ~p", [DsPrefix]),
    SigInfo = get_child(DsPrefix, "SignedInfo", Signature),
    Ref = get_child(DsPrefix, "Reference", SigInfo),
    Digest = get_child(DsPrefix, "DigestValue", Ref),
    NewDigest = Digest#xmlElement{ content = [#xmlText{value = DigestB64}]},
    NewRef = replace_child(NewDigest, Ref),
    NewSigInfo = replace_child(NewRef, SigInfo),
    logger:debug("Child: ~p~n", [NewSigInfo]),

    %% now we sign the SignedInfo element...
    SigInfoStripped = xmerl_c14n:remove_xmlns_prefixes(NewSigInfo),
    SigInfoCanon = c14n_tags(SigInfoStripped),
    SigInfoXml = xmerl:export_simple([SigInfoCanon], xmerl_xml),
    <<"<?xml version=\"1.0\"?>", Data/binary>> =
        unicode:characters_to_binary(SigInfoXml, unicode, utf8),
    logger:debug("SigInfoCanonData:~n~p~n", [Data]),

    Sig = public_key:sign(Data, HashFunction, PrivateKey),
    Sig64 = base64:encode_to_string(Sig),
    logger:debug("CertBin: ~p~n", [CertBin]),
    Cert64 = base64:encode_to_string(CertBin),
    SubjectName = subject_name(CertBin),

    %% and wrap it all up with the signature and certificate
    SigValue = get_child(DsPrefix, "SignatureValue", Signature),
    NewSigValue = SigValue#xmlElement{
                    content = [#xmlText{value = Sig64}]},
    Sig0 = replace_child(NewSigValue, Signature),
    Sig1 = replace_child(NewSigInfo, Sig0),
    KeyInfo = get_child(DsPrefix, "KeyInfo", Signature),
    X509Data = get_child(DsPrefix, "X509Data", KeyInfo),
    X509SubjectName = get_child(DsPrefix, "X509SubjectName", X509Data),
    NewX509SubjectName =
        X509SubjectName#xmlElement{
          content = [#xmlText{value = SubjectName}]},
    X509Certificate = get_child(DsPrefix, "X509Certificate", X509Data),
    NewX509Certificate =
        X509Certificate#xmlElement{
          content = [#xmlText{value = Cert64}]},
    NewX509Data = replace_child(NewX509Certificate, X509Data),
    FinalX509Data = replace_child(NewX509SubjectName, NewX509Data),
    NewKeyInfo = replace_child(FinalX509Data, KeyInfo),
    FinalSig = replace_child(NewKeyInfo, Sig1),
    ElementOut = dreplace_child(FinalSig, ElementIn),
    logger:debug("ElementOut: ~p~n", [ElementOut]),
    sign_signatures(Tail, ElementOut, PrivateKeyFun, CertBin);
sign_signatures([], ElementIn, _, _) ->
    ElementIn.

get_child(Prefix, Name, #xmlElement{name = Pname, content = Content} = _Elem) ->
    Qname = list_to_atom(Prefix ++ ":" ++ Name),
    case [ E || #xmlElement{name = N} = E <- Content, N == Qname ] of
        [] ->
            error({no_such_child, Qname, Pname});
        [Child] ->
            Child
    end.

replace_child(#xmlElement{parents = [{Pname, _}|_], pos = N} = Elem,
              #xmlElement{name = Pname, content = Content} = Parent) ->
    Len = length(Content),
    Pre = lists:sublist(Content, N-1),
    Post = lists:sublist(Content, N+1, Len - N),
    %%logger:debug("Pre: ~p~nPost: ~p~n", [Pre, Post]),
    Parent#xmlElement{content = Pre ++ [Elem | Post]}.

dreplace_child(#xmlElement{parents = Parents} = Elem, Root) ->
    logger:debug("Parents: ~p~n", [Parents]),
    do_dreplace_child(lists:reverse(Parents), Elem, Root).

do_dreplace_child([{P, _}], #xmlElement{parents = [{P, _}|_], pos = N} = Elem,
                  #xmlElement{name = P, content = Content} = Parent) ->
    Len = length(Content),
    Pre = lists:sublist(Content, N-1),
    Post = lists:sublist(Content, N+1, Len - N),
    logger:debug("PreLen: ~p~nPostLen: ~p~n", [length(Pre), length(Post)]),
    Parent#xmlElement{content = Pre ++ [Elem | Post]};
do_dreplace_child([{P, _}|T], Elem,
                  #xmlElement{name = P, content = Content} = Parent) ->
    logger:debug("P: ~p~nContent: ~p", [P, Content]),
    Parent#xmlElement{content = do_find_content(Content, T, Elem)}.

do_find_content([#xmlElement{name = P} = Parent|PTail], [{P,_}|_]=Ps, Elem) ->
    [do_dreplace_child(Ps, Elem, Parent)] ++ PTail;
do_find_content([Head | PTail], Parents, Elem) ->
    [Head] ++ do_find_content(PTail, Parents, Elem).

%% @doc Verifies an XML digital signature on the given element.
%%
%% Fingerprints is a list of valid cert fingerprints that can be
%% accepted.
%%
%% If Fingerprints is a list of #'OTPCertificate'{} it will check that
%% the cert is issued by one of the IssuerCerts in this list.
%%
%% Will throw badmatch errors if you give it XML that is not signed
%% according to the xml-dsig spec. If you're using something other
%% than rsa+sha1 or sha256 this will asplode. Don't say I didn't warn you.
%%
%% This function has now been modified to do Skatteverket's funky removing of
%% all namespace prefixes before doing a digest. Also there is function
%% that recreates bugs, which can be removed if you are not implementing
%% the Skatteverket API.
%% It will now verify all signatures inside the SOAP structure.
-spec verify(Element :: #xmlElement{},
             Fingerprints :: [#'OTPCertificate'{}] | [fingerprint()] | any) ->
          ok | {error, bad_digest | bad_signature | cert_not_accepted}.
verify(Element, Fingerprints) ->
    case xmerl_xpath:string(
           "//*[\"Signature\"=local-name() and \"http://www.w3.org/2000/"
           "09/xmldsig#\"=namespace-uri()]", Element) of
        [] ->
            {error, no_signature};
        Signatures when is_list(Signatures) ->
            verify_signatures(Signatures, Element, Fingerprints)
    end.

verify_signatures([Signature | Tail], Element, Fingerprints) ->
    %% DsNs = [{"ds", 'http://www.w3.org/2000/09/xmldsig#'},
    %%         {"ec", 'http://www.w3.org/2001/10/xml-exc-c14n#'}],
    [#xmlAttribute{value = SignatureMethodAlgorithm}] =
        xmerl_xpath:string(
          "//*[\"SignatureMethod\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]/@Algorithm", Signature),
    {HashFunction, _, _} = signature_props(SignatureMethodAlgorithm),

    [#xmlAttribute{value = "http://www.w3.org/2001/10/xml-exc-c14n#"}] =
        xmerl_xpath:string(
          "//*[\"CanonicalizationMethod\"=local-name() and \"http://www.w3.org"
          "/2000/09/xmldsig#\"=namespace-uri()]/@Algorithm", Signature),
    [#xmlAttribute{value = SignatureMethodAlgorithm}] =
        xmerl_xpath:string(
          "//*[\"SignatureMethod\"=local-name() and \"http://www.w3.org/2000/"
          "09/xmldsig#\"=namespace-uri()]/@Algorithm", Signature),
    [_C14nTx = #xmlElement{}] =
        xmerl_xpath:string(
          "//*[\"Transform\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]", Signature),
    TransformAlgo =
        xmerl_xpath:string(
          "//*[\"Transform\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]/@Algorithm", Signature),

    %% We haven't seen InclusiveNs in calls from the Skatteverket API.
    %% _InclNs = case xmerl_xpath:string("ec:InclusiveNamespaces/@PrefixList",
    %%                                  C14nTx, [{namespace, DsNs}]) of
    %%              %% FIXME! -^
    %%              [] -> [];
    %%              [#xmlAttribute{value = NsList}] -> string:tokens(NsList, " ,")
    %%          end,
    [#xmlAttribute{value = RefUri}] =
        xmerl_xpath:string(
          "//*[\"Reference\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]/@URI", Signature),
    logger:debug("Ref: ~p~n", [RefUri]),
    StrippedXml = strip_it(RefUri, TransformAlgo, Signature, Element),
    CanonishXml = xmerl:export_simple([StrippedXml], xmerl_xml),
    <<"<?xml version=\"1.0\"?>", CanonXmlUtf8/binary>> =
        unicode:characters_to_binary(CanonishXml, unicode, utf8),
    logger:debug("DigestInput:~n~ts~n --~n", [CanonXmlUtf8]),
    CanonSha = crypto:hash(HashFunction, CanonXmlUtf8),

    [#xmlText{value = Sha64}] =
        xmerl_xpath:string(
          "//*[\"DigestValue\"=local-name() and \"http://www.w3.org/2000/09/"
          "xmldsig#\"=namespace-uri()]/text()", Signature),
    CanonSha2 = base64:decode(Sha64),
    logger:debug("ComputedDigest: ~p~nDocumentDigest: ~p~n", [CanonSha, CanonSha2]),
    if not (CanonSha =:= CanonSha2) ->
            {error, {bad_digest, CanonSha, CanonSha2}};

       true ->
            [SigInfo] =
                xmerl_xpath:string(
                  "//*[\"SignedInfo\"=local-name() and \"http://www.w3.org/2000/"
                  "09/xmldsig#\"=namespace-uri()]", Signature),
            SigInfoStripped = xmerl_c14n:remove_xmlns_prefixes(SigInfo),
            SigInfoCanon = c14n_tags(SigInfoStripped),
            SigInfoXml = xmerl:export_simple([SigInfoCanon], xmerl_xml),
            <<"<?xml version=\"1.0\"?>", Data/binary>> =
                unicode:characters_to_binary(SigInfoXml, unicode, utf8),
            logger:debug("SigInfoCanonData:~n~p~n", [Data]),

            [#xmlText{value = Sig64}] =
                xmerl_xpath:string(
                  "//*[\"SignatureValue\"=local-name() and \"http://www.w3.org/2000/"
                  "09/xmldsig#\"=namespace-uri()]/text()", Signature),
            Sig = base64:decode(Sig64),

            [#xmlText{value = Cert64}] =
                xmerl_xpath:string(
                  "//*[\"X509Certificate\"=local-name() and \"http://www.w3.org/2000/"
                  "09/xmldsig#\"=namespace-uri()]/text()", Signature),
            CertBin = base64:decode(Cert64),
            CertHash = crypto:hash(sha, CertBin),
            CertHash2 = crypto:hash(sha256, CertBin),

            Cert = public_key:pkix_decode_cert(CertBin, plain),
            #'Certificate'{
               tbsCertificate =
                   #'TBSCertificate'{
                      subjectPublicKeyInfo =
                          #'SubjectPublicKeyInfo'{
                             subjectPublicKey =
                                 PubKey}}} = Cert,
            KeyBin = case PubKey of
                         {_, KeyBin2} -> KeyBin2;
                         KeyBin3 -> KeyBin3
                     end,
            Key = public_key:pem_entry_decode({'RSAPublicKey', KeyBin,
                                               not_encrypted}),
            case public_key:verify(Data, HashFunction, Sig, Key) of
                true ->
                    case Fingerprints of
                        any ->
                            verify_signatures(Tail, Element, Fingerprints);
                        [#'OTPCertificate'{}|_] ->
                            logger:debug("Cert: ~p~n",
                                      [public_key:pkix_decode_cert(CertBin, otp)]),
                            case lists:any(
                                   fun(CA) ->
                                           public_key:pkix_is_issuer(
                                             CertBin, CA)
                                   end, Fingerprints) of
                                true ->
                                    verify_signatures(Tail, Element, Fingerprints);
                                false ->
                                    {error, cert_not_accepted}
                            end;
                        _ ->
                            case lists:any(
                                   fun(X) ->
                                           lists:member(X, Fingerprints)
                                   end, [CertHash, {sha,CertHash},
                                         {sha256,CertHash2}]) of
                                true ->
                                    verify_signatures(Tail, Element, Fingerprints);
                                false ->
                                    {error, cert_not_accepted}
                            end
                    end;
                false ->
                    {error, bad_signature}
            end
    end;
verify_signatures([], _, _) ->
    ok.

replicate_bugs(#xmlElement{nsinfo = {_, "deliverSecure"}} = Element) ->
    logger:debug("replacing 'deliverSecure' with 'SealedDelivery'\n"),
    Nss = Element#xmlElement.namespace#xmlNamespace.nodes,
    {Prefix, _} = lists:keyfind('http://minameddelanden.gov.se/schema/Message/v3',
                                2, Nss),
    NewName = list_to_atom(Prefix ++ ":SealedDelivery"),
    Element#xmlElement{ name = NewName
                      , expanded_name = NewName
                      , nsinfo = {Prefix, "SealedDelivery"}
                      };
replicate_bugs(Element) ->
    Element.

%% @doc Verifies an XML digital signature, trusting any valid certificate.
%%
%% This is really not recommended for production use, but it's handy in
%% testing/development.
-spec verify(Element :: xml_thing()) -> ok | {error, bad_digest | bad_signature | cert_not_accepted}.
verify(Element) ->
    verify(Element, any).

strip_it(_Id,
         [#xmlAttribute{
             value = "http://www.w3.org/2000/09/xmldsig#enveloped-signature"}],
         Signature, Element) ->
    Parents = lists:reverse(Signature#xmlElement.parents),
    ParentStrings = [ atom_to_list(P) || {P, _} <- Parents ],
    ParentXpath = string:join(ParentStrings, "/"),
    [Parent] = xmerl_xpath:string("//"++ParentXpath, Element),
    #xmlElement{content = Kids} = Parent,
    NoSigKids = lists:filter(fun strip_sig_child/1, Kids),
    StrippedParent = Parent#xmlElement{content = NoSigKids},
    C14N = c14n_tags(StrippedParent),
    Bugged = replicate_bugs(C14N),
    Stripped = xmerl_c14n:remove_xmlns_prefixes(Bugged),
    logger:debug("Skatteverket Stripped: ~p~n", [Stripped]),
    Stripped;
strip_it("", _, _Signature, _Element) ->
    error(signature_over_everything_not_implemented);
strip_it([$# | Id],
         [#xmlAttribute{
             name = "http://www.w3.org/2001/10/xml-exc-c14n#"}],
         _Signature, Element) ->
    logger:debug("Id: ~p~n", [Id]),
    [Parent] = xmerl_xpath:string("//*[@*[local-name()='Id' and .='"++Id++"']]",
                                  Element),
    #xmlElement{content = Kids} = Parent,
    NoSigKids = lists:filter(fun strip_sig_child/1, Kids),
    StrippedParent = Parent#xmlElement{content = NoSigKids},
    C14N = c14n_tags(StrippedParent),
    Bugged = replicate_bugs(C14N),
    Stripped = xmerl_c14n:remove_xmlns_prefixes(Bugged),
    logger:debug("Skatteverket Stripped with ID: ~p~n", [Stripped]),
    Stripped.

strip_sig_child(#xmlElement{nsinfo = {_, "Signature"}}) -> false;
strip_sig_child(#xmlElement{name = 'Signature'}) -> false;
strip_sig_child(_) -> true.

c14n_tags(#xmlElement{name=Name, content=[]} = Element) ->
    %% no empy tags in c14n, this forces <foo></foo>
    logger:debug("C14N of ~p~n", [Name]),
    Element#xmlElement{content=[#xmlText{pos=1,value=""}]};
c14n_tags(#xmlElement{content=Kids} = Element) ->
    Element#xmlElement{content=c14n_tags(Kids)};
c14n_tags([H | T]) ->
    [c14n_tags(H) | c14n_tags(T)];
c14n_tags([]) ->
    [];
c14n_tags(Other) ->
    Other.

-spec signature_props(atom() | string()) -> {HashFunction :: atom(), DigestMethodUrl :: string(), SignatureMethodUrl :: string()}.
signature_props("http://www.w3.org/2000/09/xmldsig#rsa-sha1") ->
    signature_props(rsa_sha1);
signature_props(rsa_sha1) ->
    HashFunction = sha,
    DigestMethod = "http://www.w3.org/2000/09/xmldsig#sha1",
    Url = "http://www.w3.org/2000/09/xmldsig#rsa-sha1",
    {HashFunction, DigestMethod, Url};
signature_props("http://www.w3.org/2001/04/xmldsig-more#rsa-sha256") ->
    signature_props(rsa_sha256);
signature_props(rsa_sha256) ->
    HashFunction = sha256,
    DigestMethod = "http://www.w3.org/2001/04/xmlenc#sha256",
    Url = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256",
    {HashFunction, DigestMethod, Url}.

subject_name(CertBin) ->
    #'OTPCertificate'{
       tbsCertificate =
           #'OTPTBSCertificate'{
              subject = {rdnSequence, Subject}
             }} = public_key:pkix_decode_cert(CertBin, otp),
    SubjectFlat = lists:reverse(lists:flatten(Subject)),
    Parts = [ translate(A) || A <- SubjectFlat ],
    SubjectString = string:join(Parts, ";"),
    unicode:characters_to_binary(SubjectString, unicode, utf8).

translate(#'AttributeTypeAndValue'{
             type = {2,5,4,3},
             value = CN}) ->
    "CN="++quote(CN);
translate(#'AttributeTypeAndValue'{
             type = {2,5,4,10},
             value = O}) ->
    "O="++quote(O);
translate(#'AttributeTypeAndValue'{type = {2,5,4,6},
                                   value = C}) ->
    "C="++quote(C);
translate(#'AttributeTypeAndValue'{type = {2,5,4,5},
                                   value = SNr}) when is_list(SNr) ->
    "SERIALNUMBER="++quote(SNr);
translate(#'AttributeTypeAndValue'{type = {A,B,C,D},
                                   value = Value}) ->
    io_lib:format("~p.~p.~p.~p=", [A,B,C,D]) ++ quote(Value).

quote({printableString, String}) ->
    quote(String);
quote({teletexString, String}) ->
    quote(String);
quote({utf8String, Binary}) ->
    quote(unicode:characters_to_list(Binary, utf8));
quote(String) when is_list(String) ->
    case lists:member($ , String) of
        true -> "'"++String++"'";
        false -> String
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

verify_valid_sha1_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\"?><x:foo ID=\"9616e6c0-f525-11b7-afb7-5cf9dd711ed3\" xmlns:x=\"urn:foo:x:\"><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><ds:Reference URI=\"#9616e6c0-f525-11b7-afb7-5cf9dd711ed3\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><ds:DigestValue>V8qJXW/fet+RCUswqa+PSNLTG5Y=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>B5rQVbuK6861DStrYldTEdM17wkec/adr08uAXWZXU7b3r+JKt0AcDRsZUgh3dsg</ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509SubjectName>O='Default Company Ltd';2.5.4.7='Default City';C=XX</ds:X509SubjectName><ds:X509Certificate>MIIBrTCCAWegAwIBAgIJAJsPdOI20ZF2MA0GCSqGSIb3DQEBBQUAMEIxCzAJBgNVBAYTAlhYMRUwEwYDVQQHDAxEZWZhdWx0IENpdHkxHDAaBgNVBAoME0RlZmF1bHQgQ29tcGFueSBMdGQwHhcNMTMwNTAyMDYwMDM0WhcNMjMwNTAyMDYwMDM0WjBCMQswCQYDVQQGEwJYWDEVMBMGA1UEBwwMRGVmYXVsdCBDaXR5MRwwGgYDVQQKDBNEZWZhdWx0IENvbXBhbnkgTHRkMEwwDQYJKoZIhvcNAQEBBQADOwAwOAIxAM0Wz0qz1bnRjfr5+lqs2HMk+MomI/qMy5SmjJ2HBH2OgZSqjKu3mg4tPzxjRG33mwIDAQABo1AwTjAdBgNVHQ4EFgQU2XTi/8L82oGx9mcaSMggervenTowHwYDVR0jBBgwFoAU2XTi/8L82oGx9mcaSMggervenTowDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQUFAAMxAELu647IINJuZT/vxZoEgBrAwQMK+l/yam5iAWQI5Y+NtCrbC16Vu0qkLSVP5Ednrw==</ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature><x:name>blah</x:name></x:foo>", [{namespace_conformant, true}]),
    ok = verify(Doc),
    ok = verify(Doc, [<<71,188,36,243,150,120,83,146,232,73,29,239,176,116,0,
                        145,139,65,23,80>>]).

verify_valid_sha256_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\"?><saml2p:Response xmlns:saml2p=\"urn:oasis:names:tc:SAML:2.0:protocol\" Destination=\"https://api.kato.im/saml/v2/demo-okta/consume\" ID=\"id61268949075197931241532406\" IssueInstant=\"2015-01-09T01:57:56.021Z\" Version=\"2.0\"><saml2:Issuer xmlns:saml2=\"urn:oasis:names:tc:SAML:2.0:assertion\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:entity\">http://www.okta.com/kzk0hhgeJEEBMWPZLFWI</saml2:Issuer><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2001/04/xmldsig-more#rsa-sha256\"/><ds:Reference URI=\"#id61268949075197931241532406\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2001/04/xmlenc#sha256\"/><ds:DigestValue>JukfvyYSI2Vp2+ikkADKwTPBhVrYdrmBh/yF3cdZ3cU=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>3usKTy8tMunL5FkcypAYGviTWdZEscHGjtA0lpzT6LJ0yJAmHPM/2OLhCCE8E9xd/CyB4YqOvZCr+40MP/O4jE3PtcjlVozs1BlnNf/zOdusH4bMO12e6f/xKPNvtUeygS73X+1b8pgGqDUcs5UMuIlmgVjBTTVZkqEK/7naTSo=</ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509SubjectName>O='Internet Widgits Pty Ltd';2.5.4.8=Some-State;C=AU</ds:X509SubjectName><ds:X509Certificate>MIICWDCCAcGgAwIBAgIJAI8G9Einy2f5MA0GCSqGSIb3DQEBCwUAMEUxCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRlcm5ldCBXaWRnaXRzIFB0eSBMdGQwHhcNMTUwMTA5MDU1ODI4WhcNMTgwMTA4MDU1ODI4WjBFMQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDiYGHrYgEQisP8g8ZZSj2M1E6fe2McmZk1wUNtSAWU29crcp5zkvWKbrtWp+gPS1onMsBLtEBha1SHfL1XYD6FP5OSyGHRwRG6Fynz914zdEBobP2dmB+9HEMYFAzYQ5C62PVvjttqCztqk7hZaDdQT3AotWPT/oKXAm2JmSjY/wIDAQABo1AwTjAdBgNVHQ4EFgQU4hwPAoTHsONWNr8jZnr2Moqgh+8wHwYDVR0jBBgwFoAU4hwPAoTHsONWNr8jZnr2Moqgh+8wDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQsFAAOBgQDNYE6Pu6add6C5sVTc6Hn+NDJvNnIqhJNiygwHwnjqQxrafsH1SEtf4NMX9PA5zy5jjkzaZLiErCJJwZGOSDWlF5D/ZlZjKv5SazV38D7I1FPcOVDmkm0r0x+mUrI3cm6UpPf+ood+nXu5HpK5PH3qYrzNbYZKOuZU9Vfp6IUFAg==</ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature><saml2p:Status xmlns:saml2p=\"urn:oasis:names:tc:SAML:2.0:protocol\"><saml2p:StatusCode Value=\"urn:oasis:names:tc:SAML:2.0:status:Success\"/></saml2p:Status><saml2:Assertion xmlns:saml2=\"urn:oasis:names:tc:SAML:2.0:assertion\" ID=\"id61268949075925452141027347\" IssueInstant=\"2015-01-09T01:57:56.021Z\" Version=\"2.0\"><saml2:Issuer Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:entity\" xmlns:saml2=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://www.okta.com/kzk0hhgeJEEBMWPZLFWI</saml2:Issuer><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2001/04/xmldsig-more#rsa-sha256\"/><ds:Reference URI=\"#id61268949075925452141027347\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2001/04/xmlenc#sha256\"/><ds:DigestValue>n8wJPr4N7CkYugWMw1TtWc4Jt1NwwpaPvrFXbcwT2kM=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>IF6pZWHJZMoeA2t0sW+PbSEPTKpF2nan8s9NM6WbnU7RHSUmkh4f327gG37X73sBvdy9nCloPXchfKjMaTPV4pZ2p2mubAGt4wqHaS/wmHar+CJjFwMPZB0V/SSioCeG4kcjeGhfIjaL9eUMCHugcpaI2NxwN3Dt/7I9rj9PbHQ=</ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509SubjectName>O='Internet Widgits Pty Ltd';2.5.4.8=Some-State;C=AU</ds:X509SubjectName><ds:X509Certificate>MIICWDCCAcGgAwIBAgIJAI8G9Einy2f5MA0GCSqGSIb3DQEBCwUAMEUxCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRlcm5ldCBXaWRnaXRzIFB0eSBMdGQwHhcNMTUwMTA5MDU1ODI4WhcNMTgwMTA4MDU1ODI4WjBFMQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDiYGHrYgEQisP8g8ZZSj2M1E6fe2McmZk1wUNtSAWU29crcp5zkvWKbrtWp+gPS1onMsBLtEBha1SHfL1XYD6FP5OSyGHRwRG6Fynz914zdEBobP2dmB+9HEMYFAzYQ5C62PVvjttqCztqk7hZaDdQT3AotWPT/oKXAm2JmSjY/wIDAQABo1AwTjAdBgNVHQ4EFgQU4hwPAoTHsONWNr8jZnr2Moqgh+8wHwYDVR0jBBgwFoAU4hwPAoTHsONWNr8jZnr2Moqgh+8wDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQsFAAOBgQDNYE6Pu6add6C5sVTc6Hn+NDJvNnIqhJNiygwHwnjqQxrafsH1SEtf4NMX9PA5zy5jjkzaZLiErCJJwZGOSDWlF5D/ZlZjKv5SazV38D7I1FPcOVDmkm0r0x+mUrI3cm6UpPf+ood+nXu5HpK5PH3qYrzNbYZKOuZU9Vfp6IUFAg==</ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature><saml2:Subject xmlns:saml2=\"urn:oasis:names:tc:SAML:2.0:assertion\"><saml2:NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\">yaroslav@kato.im</saml2:NameID><saml2:SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"><saml2:SubjectConfirmationData NotOnOrAfter=\"2015-01-09T02:02:56.021Z\" Recipient=\"https://api.kato.im/saml/v2/demo-okta/consume\"/></saml2:SubjectConfirmation></saml2:Subject><saml2:Conditions NotBefore=\"2015-01-09T01:52:56.021Z\" NotOnOrAfter=\"2015-01-09T02:02:56.021Z\" xmlns:saml2=\"urn:oasis:names:tc:SAML:2.0:assertion\"><saml2:AudienceRestriction><saml2:Audience>https://api.kato.im/saml/v2/demo-okta/metadata</saml2:Audience></saml2:AudienceRestriction></saml2:Conditions><saml2:AuthnStatement AuthnInstant=\"2015-01-09T01:57:56.021Z\" SessionIndex=\"id1420768676021.697992970\" xmlns:saml2=\"urn:oasis:names:tc:SAML:2.0:assertion\"><saml2:AuthnContext><saml2:AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</saml2:AuthnContextClassRef></saml2:AuthnContext></saml2:AuthnStatement></saml2:Assertion></saml2p:Response>", [{namespace_conformant, true}]),
    ok = verify(Doc),
    ok = verify(Doc, [<<32,81,247,163,74,183,251,54,103,55,124,147,82,121,118,
                        45,143,9,159,4>>]).

verify_valid_skatteverket_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\" encoding=\"UTF-8\"?><p6:Envelope xmlns:p1=\"http://minameddelanden.gov.se/schema/Message\" xmlns:p2=\"http://minameddelanden.gov.se/schema/Message/v2\" xmlns:p3=\"http://minameddelanden.gov.se/schema/Message/v3\" xmlns:p4=\"http://minameddelanden.gov.se/schema/Sender\" xmlns:p5=\"http://minameddelanden.gov.se/schema/Service/v3\" xmlns:p6=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:p7=\"http://www.w3.org/2000/09/xmldsig#\"><p6:Body><p5:deliverSecure><p5:deliverSecure><p3:SignedDelivery><p3:Delivery><p3:Header><p2:Sender><p4:Id>165568402266</p4:Id><p4:Name>Kivra AB</p4:Name></p2:Sender><p2:Recipient>198101032384</p2:Recipient><p2:Reference>minimal_l</p2:Reference><p2:CorrelationId>some-uid</p2:CorrelationId><p2:Attention>198101032384</p2:Attention></p3:Header><p3:Message><p3:Header><p3:Id>some-uid</p3:Id><p3:Subject>VÃ¤lkommen till bruket</p3:Subject><p3:Supportinfo><p3:Text>Kontakta inte mÃ¤j!</p3:Text><p3:URL>http://dev.null/</p3:URL></p3:Supportinfo><p3:Language>svSE</p3:Language></p3:Header><p3:Body><p1:ContentType>text/plain</p1:ContentType><p1:Body>SMOkciDDpHIgZGluIHBkZi4=</p1:Body></p3:Body><p3:Attachment><p1:ContentType>application/pdf</p1:ContentType><p1:Body>JVBERi0xLjEKJcKlwrHDqwoKJSBNSVQgTGljZW5zZQolCiUgQ29weXJpZ2h0IChjKSAyMDEwIEJyZW5kYW4gWmFnYWVza2kKJQolIFBlcm1pc3Npb24gaXMgaGVyZWJ5IGdyYW50ZWQsIGZyZWUgb2YgY2hhcmdlLCB0byBhbnkgcGVyc29uIG9idGFpbmluZyBhIGNvcHkKJSBvZiB0aGlzIHNvZnR3YXJlIGFuZCBhc3NvY2lhdGVkIGRvY3VtZW50YXRpb24gZmlsZXMgKHRoZSAiU29mdHdhcmUiKSwgdG8gZGVhbAolIGluIHRoZSBTb2Z0d2FyZSB3aXRob3V0IHJlc3RyaWN0aW9uLCBpbmNsdWRpbmcgd2l0aG91dCBsaW1pdGF0aW9uIHRoZSByaWdodHMKJSB0byB1c2UsIGNvcHksIG1vZGlmeSwgbWVyZ2UsIHB1Ymxpc2gsIGRpc3RyaWJ1dGUsIHN1YmxpY2Vuc2UsIGFuZC9vciBzZWxsCiUgY29waWVzIG9mIHRoZSBTb2Z0d2FyZSwgYW5kIHRvIHBlcm1pdCBwZXJzb25zIHRvIHdob20gdGhlIFNvZnR3YXJlIGlzCiUgZnVybmlzaGVkIHRvIGRvIHNvLCBzdWJqZWN0IHRvIHRoZSBmb2xsb3dpbmcgY29uZGl0aW9uczoKJQolIFRoZSBhYm92ZSBjb3B5cmlnaHQgbm90aWNlIGFuZCB0aGlzIHBlcm1pc3Npb24gbm90aWNlIHNoYWxsIGJlIGluY2x1ZGVkIGluIGFsbAolIGNvcGllcyBvciBzdWJzdGFudGlhbCBwb3J0aW9ucyBvZiB0aGUgU29mdHdhcmUuCiUKJSBUSEUgU09GVFdBUkUgSVMgUFJPVklERUQgIkFTIElTIiwgV0lUSE9VVCBXQVJSQU5UWSBPRiBBTlkgS0lORCwgRVhQUkVTUyBPUgolIElNUExJRUQsIElOQ0xVRElORyBCVVQgTk9UIExJTUlURUQgVE8gVEhFIFdBUlJBTlRJRVMgT0YgTUVSQ0hBTlRBQklMSVRZLAolIEZJVE5FU1MgRk9SIEEgUEFSVElDVUxBUiBQVVJQT1NFIEFORCBOT05JTkZSSU5HRU1FTlQuIElOIE5PIEVWRU5UIFNIQUxMIFRIRQolIEFVVEhPUlMgT1IgQ09QWVJJR0hUIEhPTERFUlMgQkUgTElBQkxFIEZPUiBBTlkgQ0xBSU0sIERBTUFHRVMgT1IgT1RIRVIKJSBMSUFCSUxJVFksIFdIRVRIRVIgSU4gQU4gQUNUSU9OIE9GIENPTlRSQUNULCBUT1JUIE9SIE9USEVSV0lTRSwgQVJJU0lORyBGUk9NLAolIE9VVCBPRiBPUiBJTiBDT05ORUNUSU9OIFdJVEggVEhFIFNPRlRXQVJFIE9SIFRIRSBVU0UgT1IgT1RIRVIgREVBTElOR1MgSU4gVEhFCiUgU09GVFdBUkUuCgoxIDAgb2JqCiAgPDwgL1R5cGUgL0NhdGFsb2cKICAgICAvUGFnZXMgMiAwIFIKICA+PgplbmRvYmoKCjIgMCBvYmoKICA8PCAvVHlwZSAvUGFnZXMKICAgICAvS2lkcyBbMyAwIFJdCiAgICAgL0NvdW50IDEKICAgICAvTWVkaWFCb3ggWzAgMCAzMDAgMTQ0XQogID4+CmVuZG9iagoKMyAwIG9iagogIDw8ICAvVHlwZSAvUGFnZQogICAgICAvUGFyZW50IDIgMCBSCiAgICAgIC9SZXNvdXJjZXMKICAgICAgIDw8IC9Gb250CiAgICAgICAgICAgPDwgL0YxCiAgICAgICAgICAgICAgIDw8IC9UeXBlIC9Gb250CiAgICAgICAgICAgICAgICAgIC9TdWJ0eXBlIC9UeXBlMQogICAgICAgICAgICAgICAgICAvQmFzZUZvbnQgL1RpbWVzLVJvbWFuCiAgICAgICAgICAgICAgID4+CiAgICAgICAgICAgPj4KICAgICAgID4+CiAgICAgIC9Db250ZW50cyA0IDAgUgogID4+CmVuZG9iagoKNCAwIG9iagogIDw8IC9MZW5ndGggNTUgPj4Kc3RyZWFtCiAgQlQKICAgIC9GMSAxOCBUZgogICAgMCAwIFRkCiAgICAoSGVsbG8gV29ybGQpIFRqCiAgRVQKZW5kc3RyZWFtCmVuZG9iagoKeHJlZgowIDUKMDAwMDAwMDAwMCA2NTUzNSBmIAowMDAwMDAxMTMwIDAwMDAwIG4gCjAwMDAwMDExODkgMDAwMDAgbiAKMDAwMDAwMTI5MCAwMDAwMCBuIAowMDAwMDAxNTY5IDAwMDAwIG4gCnRyYWlsZXIKICA8PCAgL1Jvb3QgMSAwIFIKICAgICAgL1NpemUgNQogID4+CnN0YXJ0eHJlZgoxNjc3CiUlRU9GCg==</p1:Body><p1:Checksum>DDB0C170A9145291740E6D9B7253C6A1</p1:Checksum><p1:Filename>minimal_l.pdf</p1:Filename></p3:Attachment></p3:Message></p3:Delivery><p7:Signature><p7:SignedInfo><p7:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><p7:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><p7:Reference URI=\"\"><p7:Transforms><p7:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></p7:Transforms><p7:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><p7:DigestValue>8WvOHFTmsiVIKgj2x0/bPolljBM=</p7:DigestValue></p7:Reference></p7:SignedInfo><p7:SignatureValue>jORrK/6HsT3WTgsYZM0iLp6WQ8S8RPP8HRp/opIyCEM3oIxw5IvVmoLyK6nlryciQxbZRNdvdn26/MJ1DhFUTINI9aoX2GB4jv0GPf6erNF1Zs5/dy76IJYETpLnV8e4uWLgbFwO6EwgOtZjL3IfEca6qJ/UJPwek13GzkCfvBnLWEqy2YJ0K3LJ2vGUI66aowgRgFI5QQP/f7EM9rzvIeAbyk97BKurbK3qdW9Xyjdx/8dh2HBbXWSkooH2ajJJplJtT6cRZgIYhf4blDWigk97XjftkKHeQqqR2JmtFARmhaYbFQQQulauy38Gb/TIPsjircHOB/wXZSM7ALJt6w==</p7:SignatureValue><p7:KeyInfo><p7:X509Data><p7:X509SubjectName>SERIALNUMBER=165568402266;CN=testcert.kivra.net.notvalid;O='Kivra AB';C=SE</p7:X509SubjectName><p7:X509Certificate>MIIDmzCCAoOgAwIBAgIUZOoqWepECsO/IahGCnvupSvo5zAwDQYJKoZIhvcNAQELBQAwXTELMAkGA1UEBhMCU0UxETAPBgNVBAoMCEtpdnJhIEFCMSQwIgYDVQQDDBt0ZXN0Y2VydC5raXZyYS5uZXQubm90dmFsaWQxFTATBgNVBAUTDDE2NTU2ODQwMjI2NjAeFw0yMzEwMDkxMzAxMThaFw0zMzEwMDkxMzAxMThaMF0xCzAJBgNVBAYTAlNFMREwDwYDVQQKDAhLaXZyYSBBQjEkMCIGA1UEAwwbdGVzdGNlcnQua2l2cmEubmV0Lm5vdHZhbGlkMRUwEwYDVQQFEwwxNjU1Njg0MDIyNjYwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDCeMqtHCec6r+crF9NbcXjVvhY8/I6nqKlA55ftpY3INZuX/e5wivKC+hyHvOz4nIv1Jj3VfXXi6RO5R8BjCjs7umXnM3oTzsqg3F0500MAsGNHT6KlOk7cleILHEEVziPWHPX+5ZAFwgmhrSf0p2xPdzU5zEHLPmneDWCBN1E9fYIB4FIMo4ZkRf6dBmXh+FckDTcwFmbAKQLPaEy3nLxea3Q3vr5iz/xOHln04W4C4ZmIicE9HHP0QPLFrqOITBu8iv3GV3KYyJM1LWsb8AWaw19M4KbjmrCv67rm/iJncAh/O0x4qjCEFMqcEIOQ/2diOUl/tciLkdXlo1yrY8VAgMBAAGjUzBRMB0GA1UdDgQWBBS25pPvu5Tre9avga+DPxaB2jhzcDAfBgNVHSMEGDAWgBS25pPvu5Tre9avga+DPxaB2jhzcDAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3DQEBCwUAA4IBAQCfh+KodAukguzw7vMqzUrd5C/RRV5DQ1ZVLPNYX0+SdE1MuBDFDzykeOOUe/q7KHM/jLlj1WsJtlPbGaAEG3NpT3gEEUYVTXBTrzuud+fld3lMrzpbYOq6Js7r6oIuJE2r+En0gGYe2oGe90GWrWCxI+NN0/LZ5dOcKo8SupxobLSayAbf+LWaUcpyxYsAGyH51+vd8uwLVOGShGKMVZmJT1N9/eNN0uxYxWLhTlHiEnJB+fa/oXPFGZUPccwKC7pjEr78xxX1kpRHNHNMZgu22apEKCEXk6gLCgEA3/pvdbYxMvX/2f9CSaLd1tflxTDYQWs9icPEA4HDYP/ZCIIF</p7:X509Certificate></p7:X509Data></p7:KeyInfo></p7:Signature></p3:SignedDelivery><p3:Seal><p1:ReceivedTime>2023-10-11+02:00</p1:ReceivedTime><p1:SignaturesOK>true</p1:SignaturesOK></p3:Seal><p7:Signature><p7:SignedInfo><p7:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><p7:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><p7:Reference URI=\"\"><p7:Transforms><p7:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></p7:Transforms><p7:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><p7:DigestValue>n3GmKhNt2F5m3DewvYOrE8NamIc=</p7:DigestValue></p7:Reference></p7:SignedInfo><p7:SignatureValue>YT0I/YSjZIHaW+e6Spa3q0UCBFXsR6/9OfTzQByZJKp++Di0MPzRrQ0+FBF850dcanmHOyfxfa3dpYuuZ8edQtrHszsDejzFucU4PjdQ1vKxjLfEg0Ty8r7blP8XrLcaETRncaHNr7oz9xQ0pgP9/9dcGw/wGrMvICLaDcpeiqdjv0meFlZtiRG0/IwM2EdGa7gNeoYFGT0ZZTXGhmCsP7Fr704O3icOImMHZbg1trhSUd9GZLwUXgfRFEJeX9d8yn6Fh8wRiCSeF9RC8J5EYIQLTSUUuntPK7SDpOMXNzEKRES6RGw3KusiqwVbzhPDtdIWD0MmWpXBqUqSHV4mfA==</p7:SignatureValue><p7:KeyInfo><p7:X509Data><p7:X509SubjectName>SERIALNUMBER=165568402266;CN=testcert.kivra.net.notvalid;O='Kivra AB';C=SE</p7:X509SubjectName><p7:X509Certificate>MIIDmzCCAoOgAwIBAgIUZOoqWepECsO/IahGCnvupSvo5zAwDQYJKoZIhvcNAQELBQAwXTELMAkGA1UEBhMCU0UxETAPBgNVBAoMCEtpdnJhIEFCMSQwIgYDVQQDDBt0ZXN0Y2VydC5raXZyYS5uZXQubm90dmFsaWQxFTATBgNVBAUTDDE2NTU2ODQwMjI2NjAeFw0yMzEwMDkxMzAxMThaFw0zMzEwMDkxMzAxMThaMF0xCzAJBgNVBAYTAlNFMREwDwYDVQQKDAhLaXZyYSBBQjEkMCIGA1UEAwwbdGVzdGNlcnQua2l2cmEubmV0Lm5vdHZhbGlkMRUwEwYDVQQFEwwxNjU1Njg0MDIyNjYwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDCeMqtHCec6r+crF9NbcXjVvhY8/I6nqKlA55ftpY3INZuX/e5wivKC+hyHvOz4nIv1Jj3VfXXi6RO5R8BjCjs7umXnM3oTzsqg3F0500MAsGNHT6KlOk7cleILHEEVziPWHPX+5ZAFwgmhrSf0p2xPdzU5zEHLPmneDWCBN1E9fYIB4FIMo4ZkRf6dBmXh+FckDTcwFmbAKQLPaEy3nLxea3Q3vr5iz/xOHln04W4C4ZmIicE9HHP0QPLFrqOITBu8iv3GV3KYyJM1LWsb8AWaw19M4KbjmrCv67rm/iJncAh/O0x4qjCEFMqcEIOQ/2diOUl/tciLkdXlo1yrY8VAgMBAAGjUzBRMB0GA1UdDgQWBBS25pPvu5Tre9avga+DPxaB2jhzcDAfBgNVHSMEGDAWgBS25pPvu5Tre9avga+DPxaB2jhzcDAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3DQEBCwUAA4IBAQCfh+KodAukguzw7vMqzUrd5C/RRV5DQ1ZVLPNYX0+SdE1MuBDFDzykeOOUe/q7KHM/jLlj1WsJtlPbGaAEG3NpT3gEEUYVTXBTrzuud+fld3lMrzpbYOq6Js7r6oIuJE2r+En0gGYe2oGe90GWrWCxI+NN0/LZ5dOcKo8SupxobLSayAbf+LWaUcpyxYsAGyH51+vd8uwLVOGShGKMVZmJT1N9/eNN0uxYxWLhTlHiEnJB+fa/oXPFGZUPccwKC7pjEr78xxX1kpRHNHNMZgu22apEKCEXk6gLCgEA3/pvdbYxMvX/2f9CSaLd1tflxTDYQWs9icPEA4HDYP/ZCIIF</p7:X509Certificate></p7:X509Data></p7:KeyInfo></p7:Signature></p5:deliverSecure></p5:deliverSecure></p6:Body></p6:Envelope>"),
    ok = verify(Doc),
    ok = verify(Doc, [<<36,70,103,192,130,5,128,175,1,171,38,247,179,85,169,80,
                        38,39,129,100>>]).

verify_invalid_test() ->
    {Doc, _} = xmerl_scan:string("<x:foo xmlns:x=\"urn:foo:x:\"><x:name>blah</x:name></x:foo>", [{namespace_conformant, true}]),
    {error, no_signature} = verify(Doc).

verify_unknown_cert_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\"?><x:foo ID=\"9616e6c0-f525-11b7-afb7-5cf9dd711ed3\" xmlns:x=\"urn:foo:x:\"><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><ds:Reference URI=\"#9616e6c0-f525-11b7-afb7-5cf9dd711ed3\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><ds:DigestValue>V8qJXW/fet+RCUswqa+PSNLTG5Y=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>B5rQVbuK6861DStrYldTEdM17wkec/adr08uAXWZXU7b3r+JKt0AcDRsZUgh3dsg</ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509SubjectName>O='Default Company Ltd';2.5.4.7='Default City';C=XX</ds:X509SubjectName><ds:X509Certificate>MIIBrTCCAWegAwIBAgIJAJsPdOI20ZF2MA0GCSqGSIb3DQEBBQUAMEIxCzAJBgNVBAYTAlhYMRUwEwYDVQQHDAxEZWZhdWx0IENpdHkxHDAaBgNVBAoME0RlZmF1bHQgQ29tcGFueSBMdGQwHhcNMTMwNTAyMDYwMDM0WhcNMjMwNTAyMDYwMDM0WjBCMQswCQYDVQQGEwJYWDEVMBMGA1UEBwwMRGVmYXVsdCBDaXR5MRwwGgYDVQQKDBNEZWZhdWx0IENvbXBhbnkgTHRkMEwwDQYJKoZIhvcNAQEBBQADOwAwOAIxAM0Wz0qz1bnRjfr5+lqs2HMk+MomI/qMy5SmjJ2HBH2OgZSqjKu3mg4tPzxjRG33mwIDAQABo1AwTjAdBgNVHQ4EFgQU2XTi/8L82oGx9mcaSMggervenTowHwYDVR0jBBgwFoAU2XTi/8L82oGx9mcaSMggervenTowDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQUFAAMxAELu647IINJuZT/vxZoEgBrAwQMK+l/yam5iAWQI5Y+NtCrbC16Vu0qkLSVP5Ednrw==</ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature><x:name>blah</x:name></x:foo>", [{namespace_conformant, true}]),
    {error, cert_not_accepted} = verify(Doc, [<<198>>]).

verify_bad_digest_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\"?><x:foo ID=\"9616e6c0-f525-11b7-afb7-5cf9dd711ed3\" xmlns:x=\"urn:foo:x:\"><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><ds:Reference URI=\"#9616e6c0-f525-11b7-afb7-5cf9dd711ed3\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><ds:DigestValue>xPVYXCs5uMMmIbfTiTZ5R5DVhTU=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue></ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509Certificate></ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature><x:name>b1ah</x:name></x:foo>", [{namespace_conformant, true}]),
    {error, {bad_digest, _, _}} = verify(Doc).

verify_bad_signature_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\"?><x:foo ID=\"9616e6c0-f525-11b7-afb7-5cf9dd711ed3\" xmlns:x=\"urn:foo:x:\"><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><ds:Reference URI=\"#9616e6c0-f525-11b7-afb7-5cf9dd711ed3\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><ds:DigestValue>V8qJXW/fet+RCUswqa+PSNLTG5Y=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>IF6pZWHJZMoeA2t0sW+PbSEPTKpF2nan8s9NM6WbnU7RHSUmkh4f327gG37X73sBvdy9nCloPXchfKjMaTPV4pZ2p2mubAGt4wqHaS/wmHar+CJjFwMPZB0V/SSioCeG4kcjeGhfIjaL9eUMCHugcpaI2NxwN3Dt/7I9rj9PbHQ=</ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509SubjectName>O='Default Company Ltd';2.5.4.7='Default City';C=XX</ds:X509SubjectName><ds:X509Certificate>MIIBrTCCAWegAwIBAgIJAJsPdOI20ZF2MA0GCSqGSIb3DQEBBQUAMEIxCzAJBgNVBAYTAlhYMRUwEwYDVQQHDAxEZWZhdWx0IENpdHkxHDAaBgNVBAoME0RlZmF1bHQgQ29tcGFueSBMdGQwHhcNMTMwNTAyMDYwMDM0WhcNMjMwNTAyMDYwMDM0WjBCMQswCQYDVQQGEwJYWDEVMBMGA1UEBwwMRGVmYXVsdCBDaXR5MRwwGgYDVQQKDBNEZWZhdWx0IENvbXBhbnkgTHRkMEwwDQYJKoZIhvcNAQEBBQADOwAwOAIxAM0Wz0qz1bnRjfr5+lqs2HMk+MomI/qMy5SmjJ2HBH2OgZSqjKu3mg4tPzxjRG33mwIDAQABo1AwTjAdBgNVHQ4EFgQU2XTi/8L82oGx9mcaSMggervenTowHwYDVR0jBBgwFoAU2XTi/8L82oGx9mcaSMggervenTowDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQUFAAMxAELu647IINJuZT/vxZoEgBrAwQMK+l/yam5iAWQI5Y+NtCrbC16Vu0qkLSVP5Ednrw==</ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature><x:name>blah</x:name></x:foo>", [{namespace_conformant, true}]),
    {error, bad_signature} = verify(Doc).

test_sign_key() ->
    CertBin = <<48,130,1,173,48,130,1,103,160,3,2,1,2,2,9,0,155,15,116,226,54,
                209,145,118,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,48,66,49,
                11,48,9,6,3,85,4,6,19,2,88,88,49,21,48,19,6,3,85,4,7,12,12,68,
                101,102,97,117,108,116,32,67,105,116,121,49,28,48,26,6,3,85,4,
                10,12,19,68,101,102,97,117,108,116,32,67,111,109,112,97,110,
                121,32,76,116,100,48,30,23,13,49,51,48,53,48,50,48,54,48,48,51,
                52,90,23,13,50,51,48,53,48,50,48,54,48,48,51,52,90,48,66,49,11,
                48,9,6,3,85,4,6,19,2,88,88,49,21,48,19,6,3,85,4,7,12,12,68,101,
                102,97,117,108,116,32,67,105,116,121,49,28,48,26,6,3,85,4,10,
                12,19,68,101,102,97,117,108,116,32,67,111,109,112,97,110,121,
                32,76,116,100,48,76,48,13,6,9,42,134,72,134,247,13,1,1,1,5,0,3,
                59,0,48,56,2,49,0,205,22,207,74,179,213,185,209,141,250,249,
                250,90,172,216,115,36,248,202,38,35,250,140,203,148,166,140,
                157,135,4,125,142,129,148,170,140,171,183,154,14,45,63,60,99,
                68,109,247,155,2,3,1,0,1,163,80,48,78,48,29,6,3,85,29,14,4,22,
                4,20,217,116,226,255,194,252,218,129,177,246,103,26,72,200,32,
                122,187,222,157,58,48,31,6,3,85,29,35,4,24,48,22,128,20,217,
                116,226,255,194,252,218,129,177,246,103,26,72,200,32,122,187,
                222,157,58,48,12,6,3,85,29,19,4,5,48,3,1,1,255,48,13,6,9,42,
                134,72,134,247,13,1,1,5,5,0,3,49,0,66,238,235,142,200,32,210,
                110,101,63,239,197,154,4,128,26,192,193,3,10,250,95,242,106,
                110,98,1,100,8,229,143,141,180,42,219,11,94,149,187,74,164,45,
                37,79,228,71,103,175>>,
    Key = {'RSAPrivateKey','two-prime',
           31566101599917470453416065772975030637050267921499643485243561060280673467204714198784209398028051515492879184033691,
           65537,
           18573989898799417322963879097353191425554564320258643998367520268996258880659389403428515182780052189009731243940089,
           6176779427556368800436097873318862403597526763704995657789,
           5110446628398630915379329225736384395133647699411033691319,
           3629707330424811560529090457257061337677158715287651140161,
           3337927863271614430989022488622788202360360154126504237157,
           3289563093010152325531764796397097457944832648507910197015,
           asn1_NOVALUE},
    KeyFun = fun() -> Key end,
    {KeyFun, CertBin}.

test_sign_256_key() ->
    CertBin = <<48,130,2,88,48,130,1,193,160,3,2,1,2,2,9,0,143,6,244,72,167,203,103,249,48,
                     13,6,9,42,134,72,134,247,13,1,1,11,5,0,48,69,49,11,48,9,6,3,85,4,6,19,2,65,
                     85,49,19,48,17,6,3,85,4,8,12,10,83,111,109,101,45,83,116,97,116,101,49,33,48,
                     31,6,3,85,4,10,12,24,73,110,116,101,114,110,101,116,32,87,105,100,103,105,
                     116,115,32,80,116,121,32,76,116,100,48,30,23,13,49,53,48,49,48,57,48,53,53,
                     56,50,56,90,23,13,49,56,48,49,48,56,48,53,53,56,50,56,90,48,69,49,11,48,9,6,
                     3,85,4,6,19,2,65,85,49,19,48,17,6,3,85,4,8,12,10,83,111,109,101,45,83,116,97,
                     116,101,49,33,48,31,6,3,85,4,10,12,24,73,110,116,101,114,110,101,116,32,87,
                     105,100,103,105,116,115,32,80,116,121,32,76,116,100,48,129,159,48,13,6,9,42,
                     134,72,134,247,13,1,1,1,5,0,3,129,141,0,48,129,137,2,129,129,0,226,96,97,235,
                     98,1,16,138,195,252,131,198,89,74,61,140,212,78,159,123,99,28,153,153,53,193,
                     67,109,72,5,148,219,215,43,114,158,115,146,245,138,110,187,86,167,232,15,75,
                     90,39,50,192,75,180,64,97,107,84,135,124,189,87,96,62,133,63,147,146,200,97,
                     209,193,17,186,23,41,243,247,94,51,116,64,104,108,253,157,152,31,189,28,67,
                     24,20,12,216,67,144,186,216,245,111,142,219,106,11,59,106,147,184,89,104,55,
                     80,79,112,40,181,99,211,254,130,151,2,109,137,153,40,216,255,2,3,1,0,1,163,
                     80,48,78,48,29,6,3,85,29,14,4,22,4,20,226,28,15,2,132,199,176,227,86,54,191,
                     35,102,122,246,50,138,160,135,239,48,31,6,3,85,29,35,4,24,48,22,128,20,226,
                     28,15,2,132,199,176,227,86,54,191,35,102,122,246,50,138,160,135,239,48,12,6,
                     3,85,29,19,4,5,48,3,1,1,255,48,13,6,9,42,134,72,134,247,13,1,1,11,5,0,3,129,
                     129,0,205,96,78,143,187,166,157,119,160,185,177,84,220,232,121,254,52,50,111,
                     54,114,42,132,147,98,202,12,7,194,120,234,67,26,218,126,193,245,72,75,95,224,
                     211,23,244,240,57,207,46,99,142,76,218,100,184,132,172,34,73,193,145,142,72,
                     53,165,23,144,255,102,86,99,42,254,82,107,53,119,240,62,200,212,83,220,57,80,
                     230,146,109,43,211,31,166,82,178,55,114,110,148,164,247,254,162,135,126,157,
                     123,185,30,146,185,60,125,234,98,188,205,109,134,74,58,230,84,245,87,233,232,
                     133,5,2>>,
    Key = {'RSAPrivateKey', 'two-prime',
                                    158966980232852666772927195913239826068125056530979279609712979168793279569950881734703825673400914686519075266453462906345312980842795804140929898282998881309114359443174166979208804324900933216050217378336424610098894747923637370129796798783736195833452722831496313972485597624172644388752444143966442019071,
                                    65537,
                                    81585278241787073666896657377387148477980168094656271566789692148593343582026914676392925775132211811359523575799353416465883426318681613016771856031686932947271317419547861320644294073546214321361245588222429356422579589512434099189282561422126611592192445638395200306602306031474495398876927483244443369593,
                                    12815152123986810526369994227491082588178787406540561310765978351462418958697931052574961306076834858513248417634296430722377133684866082077619514584491459,
                                    12404611251965211323458298415076779598256259333742031592133644354834252221601927657224330177651511823990769238743820731690160529549534378492093966021787669,
                                    12713470949925240093275522448216850277486308815036508762104942467263257296453352812079684136246663289377845680597663167924634849028624106358859697266275251,
                                    6810924077860081545742457087875899675964008664805732102649450821129373208143854079642954317600927742717607462760847234526126256852014054284747688684682049,
                                    4159324767638175662417764641421395971040638684938277905991804960733387537828956767796004537366153684030130407445292440219293856342103196426697248208199489,
                                    asn1_NOVALUE},
    KeyFun = fun() -> Key end,
    {KeyFun, CertBin}.


sign_and_verify_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\" encoding=\"utf-8\"?><x:foo id=\"test\" xmlns:x=\"urn:foo:x:\"><x:name>blah</x:name><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><ds:Reference URI=\"\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><ds:DigestValue></ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue></ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509SubjectName></ds:X509SubjectName><ds:X509Certificate></ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature></x:foo>", [{namespace_conformant, true}]),
    {Key, CertBin} = test_sign_key(),
    SignedXml = sign(Doc, Key, CertBin),
    ok = verify(SignedXml, [crypto:hash(sha, CertBin)]).

sign_and_verify_sha256_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\" encoding=\"utf-8\"?><x:foo id=\"test\" xmlns:x=\"urn:foo:x:\"><x:name>blah</x:name><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><ds:Reference URI=\"\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><ds:DigestValue></ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue></ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509SubjectName></ds:X509SubjectName><ds:X509Certificate></ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature></x:foo>", [{namespace_conformant, true}]),
    {Key, CertBin} = test_sign_256_key(),
    SignedXml = sign(Doc, Key, CertBin),
    ok = verify(SignedXml, [crypto:hash(sha, CertBin)]).

%% We don't generate id's in this implementation
%% sign_generate_id_test() ->
%%     {Doc, _} = xmerl_scan:string("<x:foo xmlns:x=\"urn:foo:x:\"><x:name>blah</x:name></x:foo>", [{namespace_conformant, true}]),
%%     {Key, CertBin} = test_sign_key(),
%%     SignedXml = sign(Doc, Key, CertBin),
%%     Ns = [{"ds", 'http://www.w3.org/2000/09/xmldsig#'}],
%%     [#xmlAttribute{name = 'ID', value = RootId}] = xmerl_xpath:string("@ID", SignedXml, [{namespace, Ns}]),
%%     [#xmlAttribute{value = "#" ++ RootId}] = xmerl_xpath:string("ds:Signature/ds:SignedInfo/ds:Reference/@URI", SignedXml, [{namespace, Ns}]).

utf8_test() ->
    Name = <<"Игорь Карымов "/utf8>>,
    ThisPerson = <<"その人\n"/utf8>>,
    XmlData = <<"<x:foo xmlns:x=\"urn:foo:x#\"><x:name attr=\"",Name/binary,"\">",ThisPerson/binary,"</x:name><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/><ds:Reference URI=\"\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/><ds:DigestValue></ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue></ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509SubjectName></ds:X509SubjectName><ds:X509Certificate></ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature></x:foo>">>,
    {Doc, _} = xmerl_scan:string(binary_to_list(XmlData), [{namespace_conformant, true}]),
    {Key, CertBin} = test_sign_key(),
    SignedXml = sign(Doc, Key, CertBin),
    Ns = [{"ds", 'http://www.w3.org/2000/09/xmldsig#'}, {"x", 'urn:foo:x#'}],
    AttrValue = unicode:characters_to_list(Name),
    [#xmlAttribute{name = 'attr', value = AttrValue}] =
        xmerl_xpath:string("x:name/@attr", SignedXml, [{namespace, Ns}]),
    TextValue = unicode:characters_to_list(ThisPerson),
    [#xmlText{value = TextValue}] =
        xmerl_xpath:string("x:name/text()", SignedXml, [{namespace, Ns}]),
    ok = verify(SignedXml, [crypto:hash(sha, CertBin)]).

-endif.
