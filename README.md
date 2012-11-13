# JSON Validator
JSON Schema implementation in Erlang and standalone validation server.

## Standalone mode
* Put your schemas in `webroot` directory. May use subdirectories too. 
([Great tool for generating JSON schema from JSON](http://www.jsonschema.net/))
* Start server (it listens on port 5000 by default, can be changed in `etc/app.config`)
* `GET` request will just receive schema from server
* `POST` request to schema will validate JSON sent as request body. 
Response will contain JSON-encoded array of validation errors or empty array if validation passes.

## Standalone mode example
Let's assume that webroot directory has following structure:

    webroot
     |-- schemas_v1
     |    |-- schema1.jsonschema
     |    +-- schema2.jsonschema
     |
     +-- schemas_v2
          |-- schema1.jsonschema
          +-- schema2.jsonschema

* Request `GET localhost:5000/schemas_v1/schema1.jsonschema` will receive `webroot/schemas_v1/schema1.jsonschema`
* Posting some JSON to `POST localhost:5000/schemas_v1/schema1.jsonschema` will validate posted JSON against schema
in `webroot/schemas_v1/schema1.jsonschema` file and return validation errors in response.

## Using as library
You may use JSON Validator as library. Required sources are `jvalidator_validator.erl` and all `constraint_*.erl` files.
To perform validation you need to decode your JSON and JSON schema by `mochijson2:decode/1` function 
and pass them to `jvalidator_validator:validate/2` function.

## Requires
* [Mochiweb](https://github.com/mochi/mochiweb)