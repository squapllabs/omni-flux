pipeline {
    agent any

 environment{
     GITHUB_TOKEN = credentials('your-github-token-credential-id')
}
    stages {
        stage('checkout code'){
            steps {
                checkout([$class: 'GitSCM',
                branches: [[name: '*/main']],
                extensions: scm.extensions,
                userRemoteConfigs: [[
                   credentialsId: env.GITHUB_TOKEN,
                    url: 'https://github.com/squapllabs/omni-flux',
                ]]
            ])
            echo 'git checkout completed'
            }
		}
        stage('docker image build') {
            steps {
                echo 'running docker container details'
                sh 'docker ps'
				sh 'whoami'
                echo 'docker build started'
                sh 'docker build -t Omni_flux_image .'
                echo 'docker build completed'
            }
        }
    }
}
