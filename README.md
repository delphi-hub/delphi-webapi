# Delphi Web API <img src="https://github.com/delphi-hub/delphi/raw/master/img/delphi.png" align="right" height=140/>

The web API implementation for the Delphi platform.

We are currently in pre-alpha state! There is no release and the code in
this repository is purely experimental!

|branch | status | codacy | coverage | snyk |
| :---: | :---: | :---: | :---: |  :---: |  
| master | [![Build Status](https://travis-ci.org/delphi-hub/delphi-webapi.svg?branch=master)](https://travis-ci.org/delphi-hub/delphi-webapi) | [![Codacy Badge](https://api.codacy.com/project/badge/Grade/8ebe27850ffb4139af6280fd1cd6d540)](https://www.codacy.com/app/delphi-hub/delphi-webapi?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=delphi-hub/delphi-webapi&amp;utm_campaign=Badge_Grade)| [![Codacy Badge](https://api.codacy.com/project/badge/Coverage/8ebe27850ffb4139af6280fd1cd6d540)](https://www.codacy.com/manual/delphi-hub/delphi-webapi?utm_source=github.com&utm_medium=referral&utm_content=delphi-hub/delphi-webapi&utm_campaign=Badge_Coverage) | [![Known Vulnerabilities](https://snyk.io/test/github/delphi-hub/delphi-webapi/badge.svg?targetFile=build.sbt)](https://snyk.io/test/github/delphi-hub/delphi-webapi/?targetFile=build.sbt) |
| develop | [![Build Status](https://travis-ci.org/delphi-hub/delphi-webapi.svg?branch=develop)](https://travis-ci.org/delphi-hub/delphi-webapi) | [![Codacy Badge](https://api.codacy.com/project/badge/Grade/8ebe27850ffb4139af6280fd1cd6d540?branch=develop)](https://www.codacy.com/app/delphi-hub/delphi-webapi?branch=develop&amp;utm_source=github.com&amp;utm_medium=referral&amp;utm_content=delphi-hub/delphi-webapi&amp;utm_campaign=Badge_Grade) | [![Codacy Badge](https://api.codacy.com/project/badge/Coverage/8ebe27850ffb4139af6280fd1cd6d540?branch=develop)](https://www.codacy.com/manual/delphi-hub/delphi-webapi?branch=develop&utm_source=github.com&utm_medium=referral&utm_content=delphi-hub/delphi-webapi&utm_campaign=Badge_Coverage) | [![Known Vulnerabilities](https://snyk.io/test/github/delphi-hub/delphi-webapi/develop/badge.svg?targetFile=build.sbt)](https://snyk.io/test/github/delphi-hub/delphi-webapi/develop/?targetFile=build.sbt) |

## What is the Delphi Web API?

It is the primary access point to the Delphi system.

## How does it work?

The Delphi Web API communicates with the underlying Elasticsearch database to provide access to data.

## How can I use it?

If you just wish to query the results, maybe the public instance at https://delphi.cs.uni-paderborn.de is the right choice for you.

If you want to run your own infrastructure, you can start the web API
```
sbt run
```

It expects a running instance of elasticsearch on port 9200 on the same machine.

## Community

Feel welcome to join our chatroom on Gitter: [![Join the chat at https://gitter.im/delphi-hub/delphi](https://badges.gitter.im/delphi-hub/delphi.svg)](https://gitter.im/delphi-hub/delphi?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## Contributing

Contributions are *very* welcome!

Before contributing, please read our [Code of Conduct](CODE_OF_CONDUCT.md).

Refer to the [Contribution Guide](CONTRIBUTING.md) for details about the workflow.
We use Pull Requests to collect contributions. Especially look out for "help wanted" issues
[![GitHub issues by-label](https://img.shields.io/github/issues/delphi-hub/delphi-webapi/help%20wanted.svg)](https://github.com/delphi-hub/delphi-webapi/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22),
but feel free to work on other issues as well.
You can ask for clarification in the issues directly, or use our Gitter
chat for a more interactive experience.

[![GitHub issues](https://img.shields.io/github/issues/delphi-hub/delphi-webapi.svg)](https://github.com/delphi-hub/delphi-webapi/issues)


## License

The Delphi Web API is open source and available under Apache 2 License.

[![GitHub license](https://img.shields.io/github/license/delphi-hub/delphi-webapi.svg)](https://github.com/delphi-hub/delphi-webapi/blob/master/LICENSE)
