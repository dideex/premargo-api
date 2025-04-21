# OpenAPI Generator Pipeline

This repository implements a contract-first API development approach using OpenAPI Generator to generate server and client code from an OpenAPI specification.

## Overview

The pipeline automatically processes an OpenAPI specification file and generates:

- Erlang server implementation
- Python client SDK
- Swagger UI documentation

The generated code is pushed to separate branches, making it easy to integrate into your development workflow.

## How It Works

The GitLab CI/CD pipeline automates the following process:

1. **Validation**: Validates the OpenAPI specification file to ensure it complies with the OpenAPI standard
2. **Version Extraction**: Extracts the API version from the specification file
3. **Code Generation**:
   - Generates Erlang server code
   - Generates Python client SDK
4. **Swagger UI**: Creates a Swagger UI interface for API documentation
5. **Distribution**: Pushes the generated code to dedicated branches:
   - [`erlang_server`](https://gitlab.exan.tech/erlang/premargo_api/-/tree/erlang_server) for the Erlang server implementation
   - [`python_client`](https://gitlab.exan.tech/erlang/premargo_api/-/tree/python_client) for the Python client SDK

## Technology

The pipeline uses:

- OpenAPI Generator CLI (version 2.19.0)
- GitLab CI/CD for automation
- Swagger UI for API documentation

## Contract-First Development

This pipeline supports a contract-first development methodology, where:

1. API is designed and documented first in an OpenAPI specification
2. Server and client implementations are automatically generated
3. Developers focus on business logic implementation rather than API structure

This approach ensures consistency between API documentation, server implementation, and client SDKs.

## Usage

To use this pipeline:

1. Update the `schema.yaml` OpenAPI specification file
2. **Important**: If you want to see changes in the generated files, you must update the `info.version` field in `schema.yaml`. The pipeline uses this version to determine whether to regenerate code.
3. Push changes to the repository
4. The pipeline will automatically validate, generate code, and push to the appropriate branches
5. Access the generated code from the respective branches:
   - [Erlang Server Implementation](https://gitlab.exan.tech/erlang/premargo_api/-/tree/erlang_server)
   - [Python Client SDK](https://gitlab.exan.tech/erlang/premargo_api/-/tree/python_client)

## Erlang Server Configuration

The generated Erlang server provides a `start/2` function with the following options:

```erlang
start(ID, Params) -> {ok, pid()} | {error, any()}
```

Where `Params` is a map with the following options:

- `transport`: The transport protocol to use (`tcp` or `ssl`)
- `transport_opts`: Transport options for Ranch (the underlying connection handler)
- `swagger_json_handler`: Handler for serving the OpenAPI specification
- `protocol_opts`: Protocol options for Cowboy (the HTTP server)
- `service_routes`: Additional routes to add to the server
- `logic_handler`: Module implementing the business logic (defaults to `{packageName}_logic_handler`)

Example usage:

```erlang
{ok, _} = my_api_server:start(http, #{
    transport => tcp,
    transport_opts => #{port => 8080},
    logic_handler => my_custom_logic_handler
}).
```

## Configuration

The pipeline is configured through environment variables in the `.gitlab-ci.yml` file:

- `OPENAPI_GENERATOR_VERSION`: Version of the OpenAPI Generator CLI
- `SPEC_FILE`: Path to the OpenAPI specification file
- `ERLANG_BRANCH`: Branch for Erlang server code
- `PYTHON_BRANCH`: Branch for Python client code
