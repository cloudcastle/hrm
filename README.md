# HRM - Hardware Resource Manager

Daemon for optimal EC2 resources utilization.

Provides API and conventions for background jobs to be performed on dedicated EC2
instance while keeping it's utilization as low as possible.

It performs smart starting and stopping instances based on scheduled jobs.

## API

### `POST /tasks` — enqueue job. Params:

- `action_url` — Required. URL of the job worker. It should start job upon request and keep connection until completion;
- `callback_url` — Optional. URL to call back upon completion or error;
- `instance_id` — Optional. EC2 Instance ID to start/stop;
- `access_key_id`, `access_key_secret` — Required if `instance_id` specified. Access Key for that instance.

Returns ID to identify that job later.

Use `{dns_name}` or `{private_dns_name}` as a part of `action_url` to use real instance hostname, i.e. `http://#{private_dns_name}/my_action_url`

### `GET /tasks/:id` — check job status.

Returns JSON representation of job params, including `status` and `meta` (see below).

### `PUT /tasks/:id` — update job `meta`. Params:

- `meta` — Any information worker may want to report. It's intended mainly for progress information.

### `DELETE /tasks/:id` — delete job.

Just in case :)

### `GET /status` — check status of all managed jobs and instances.

Returns JSON with:
- `controlled_instances` array – instances started by HRM and scheduled for stopping
- `pending_tasks` hash {"task id": "instance id"} — tasks that are running currently.

## Other details

- Additional param `hrm_task_id` is being added to `action_url` query string for job worker to know how to update meta.
- Additional param `hrm_task_id` is being added to `callback_url` query string for client to know which job is done.
- `hrm_task` param contains same JSON data as `GET /tasks/:id` result
- HTTP port could be changed in `dev.config`

## Usage

- Install Erlang R15B+ for your system
- Checkout this repo
- `./rebar get-deps`
- `make`

- `./start` to start it as daemon
- `make shell` to start it interactively

## Todo

- wipe `access_key_id` and `access_key_secret` when it's not needed anymore
- some way to get monit (or some other monitoring tool) play well with daemon
