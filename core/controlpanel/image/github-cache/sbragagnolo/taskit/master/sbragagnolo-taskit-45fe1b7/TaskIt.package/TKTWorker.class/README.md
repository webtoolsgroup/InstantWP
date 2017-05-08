! TKTWorker

The worker runner, instance of TKTWorker, is a task runner that uses a single process to execute tasks from a queue. The worker's single process removes one-by-one the tasks from the queue and executes them sequenceally. Then, schedulling a task into a worker means to add the task inside the queue.

A worker manages the life-cycle of its process and provides the messages start and stop to control when the worker thread will begin and end.

[[[language=smalltalk
worker := TKTWorker new.
worker start.
worker schedule: [ 1 + 5 ].
worker stop.
]]]

By using workers, we can control the amount of alive processes and how tasks are distributed amongst them. For example, in the following example three tasks are executed sequenceally in a single separate process while still allowing us to use an asynchronous style of programming.

[[[language=smalltalk
worker := TKTWorker new start.
future1 := worker future: [ 2 + 2 ].
future2 := worker future: [ 3 + 3 ].
future3 := worker future: [ 1 + 1 ].
]]]

Workers can be combined into worker pools.