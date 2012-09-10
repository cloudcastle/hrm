# HRM - Hardware Resource Manager

Daemon for optimal EC2 resources utilization.

Provides API and conventions for background jobs to be performed on dedicated EC2
instance while keeping it's utilization as low as possible.

It performs smart starting and stopping instances based on scheduled jobs.

## API

`POST /tasks` — enqueue job. Params:

- `action_url` — URL of the job worker. It should start job upon request and keep connection until completion;
- `callback_url` — URL to call back upon completion or error;
- `instance_id` — Optional, EC2 Instance ID to start/stop;
- `access_key_id`, `access_key_secret` — Access Key for that instance.

Returns ID to identify that job later.

`GET /tasks/:id` — check job status.

Returns JSON representation of job params, including `status` and `meta` (see below).

`PUT /tasks/:id` — update job `meta`. Params:

- `meta` — Any information worker may want to report. It's intended mainly for progress information.

`DELETE /tasks/:id` — delete job. Just in case :)

## Other details

- Additional param `hrm_task_id` is being added to `action_url` query string for job worker to know how to update meta.
- Additional params `hrm_task_id` and `hrm_task` is being added to `callback_url` query string for client to know which job is done.
- `hrm_task` param contains same JSON data as `GET /tasks/:id` result

## Usage

- Install latest Erlang for your system
- Checkout `hrm` project
- `rebar get-deps`
- `make`

- `./start` to start it as daemon
- `make shell` to start it interactively
