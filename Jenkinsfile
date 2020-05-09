node {
    def app

    stage('Clone repository') {
        /* Let's make sure we have the repository cloned to our workspace */

        checkout scm
    }

    stage('Build') {
        /* This builds the actual image; synonymous to
         * docker build on the command line */
        sh 'cd parking_lot && ./rebar3 compile'
    }

    stage('Test') {
        app.inside {
            sh './rebar3 eunit'
        }
    }

}
