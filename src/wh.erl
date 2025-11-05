%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2013-2017 Campanja
%%% Copyright (c) 2017-2020 [24]7.ai
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
%%% ---------------------------------------------------------------------------
%%% XML helper library
%%% Doesn't support: group, attributeGroup, include and notation types
%%% ---------------------------------------------------------------------------

-module(wh).

-export([get_child/2, get_children/2, find_element/2,
         get_all_children/1, get_all_child_elements/1,
         get_attribute/2, get_name/1, get_simple_name/1,
         get_text/1, get_docs/1]).

-include("ews.hrl").

%% >-----------------------------------------------------------------------< %%

get_attribute(undefined, _) -> undefined;
get_attribute(#xmlElement{attributes=Attrs, namespace=Ns}, Name) ->
    case [ V || #xmlAttribute{name=N, value=V} <- Attrs, Name == N ] of
        [] ->
            undefined;
        [Attr] ->
            IsHttp = lists:prefix("http", Attr),
            case lists:member($:, Attr) of
                true when not IsHttp ->
                    [Tns, BaseName] = string:tokens(Attr, ":"),
                    case lists:keyfind(Tns, 1
                                      , Ns#xmlNamespace.nodes) of
                        {_, Uri} ->
                            {atom_to_list(Uri), BaseName};
                        %% FIXME: do something smarter to find attrs like
                        %% "[\\w\\d\\p{Zs}\\-/_.,:]*"
                        false ->
                            Attr
                    end;
                _ ->
                    Attr
            end
    end.

get_child(Xml, Name) ->
    case get_children(Xml, Name) of
        [] ->
            undefined;
        [Child|_] ->
            Child
    end.

get_children(#xmlElement{content=Content}, Name) ->
    [ E || E = #xmlElement{nsinfo={_, N}} <- Content, N == Name ] ++
    [ E || E = #xmlElement{name=N} <- Content, atom_to_list(N) == Name ].

find_element(#xmlElement{content=Content} = Element, Name) ->
    case get_child(Element, Name) of
        undefined ->
            find_element(Content, Name);
        Child ->
            Child
    end;
find_element([E | Elements], Name) ->
    case find_element(E, Name) of
        undefined ->
            find_element(Elements, Name);
        Child ->
            Child
    end;
find_element([], _Name) ->
    undefined;
find_element(_, _Name) ->
    undefined.

get_all_children(undefined) -> undefined;
get_all_children(#xmlElement{content=Content}) -> Content.

get_all_child_elements(undefined) ->
    undefined;
get_all_child_elements(#xmlElement{content=Content}) ->
    [ E || E = #xmlElement{} <- Content ].

get_name(#xmlElement{expanded_name=N}) -> N.

get_simple_name(#xmlElement{nsinfo={_, N}}) when N /= undefined ->
    N;
get_simple_name(#xmlElement{expanded_name={_, N}}) when N /= undefined ->
    atom_to_list(N);
get_simple_name(#xmlElement{name=N}) ->
    stripns(N).

get_text(#xmlElement{content=Texts}) ->
    [ T || #xmlText{value=T} <- Texts ].

get_docs(ParentElem) ->
    case find_element(ParentElem, "documentation") of
        undefined ->
            undefined;
        Child ->
            unicode:characters_to_binary(get_text(Child))
    end.

stripns(Name) when is_atom(Name) ->
    stripns(atom_to_list(Name));
stripns(Name) when is_list(Name) ->
    case lists:member($:, Name) of
        true ->
            [_,NewName] = string:tokens(Name, ":"),
            NewName;
        false ->
            Name
    end.
