// -*- groovy -*-
pipeline {
    agent {
        label 'erlang-21'
    }

    stages {
        stage('Compile') {
            steps {
                sshagent (credentials: ['git']) {
                    sh 'make'
                }
            }
        }
        stage('Xref') {
            steps {
                sshagent (credentials: ['git']) {
                    sh 'make xref'
                }
            }
        }
        // stage('EUnit') {
        //     steps {
        //         sshagent (credentials: ['git']) {
        //             sh 'make eunit'
        //         }
        //     }
        // }
        stage('Common Test') {
            steps {
                sshagent (credentials: ['git']) {
                    sh 'make ct'
                }
            }
        }
        stage('Dialyzer') {
            steps {
                sshagent (credentials: ['git']) {
                    sh 'make dialyzer'
                }
            }
        }
        // stage('Build and push image') {
        //     steps {
        //         sshagent (credentials: ['git']) {
        //             sh 'make push'
        //         }
        //     }
        // }
    }
}
