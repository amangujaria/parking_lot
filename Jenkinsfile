node {
    def app

    stage('checkout') {
        /* Let's make sure we have the repository cloned to our workspace */

        checkout scm
    }

    stage('Build') {
        sh 'cd parking_lot && ./rebar3 compile'
    }

    stage('Test') {
        echo 'Testing'
    }

}
