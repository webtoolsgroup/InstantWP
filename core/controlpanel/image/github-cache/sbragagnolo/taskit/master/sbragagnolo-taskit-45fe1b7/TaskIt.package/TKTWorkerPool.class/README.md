! TKTWorkerPool

A TaskIT worker pool is pool of worker runners, equivalent to a ThreadPool from other programming languages. Its main purpose is to provide several worker runners and decouple us from the management of threads/processes. A worker pool is a runner in the sense we use the schedule: message to schedule tasks in it. Internally, all runners inside a worker pool share a single task queue.

Different applications may have different concurrency needs, thus, TaskIT worker pools do not provide a default amount of workers. Before using a pool, we need to specify the maximum number of workers in the pool using the poolMaxSize: message. A worker pool will create new workers on demand.

[[[language=smalltalk
pool := TKTWorkerPool new.
pool poolMaxSize: 5.
]]]

TaskIT worker pools use internally an extra worker to synchronize the access to its task queue. Because of this, a worker pool has to be manually started using the start message before scheduled messages start to be executed.

[[[language=smalltalk
pool := TKTWorkerPool new.
pool poolMaxSize: 5.
pool start.
pool schedule: [ 1 logCr ].
]]]

Once we are done with the worker pool, we can stop it by sending it the stop message.

[[[language=smalltalk
pool stop.
]]]