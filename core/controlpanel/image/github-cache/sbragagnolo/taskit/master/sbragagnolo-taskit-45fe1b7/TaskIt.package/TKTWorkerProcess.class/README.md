I am a task executer paired with a worker. A worker-workerProcess pair shares the same taskQueue so the worker can schedule on it and I can execute them.

The reason I exist is that if the Pharo process executing the worker's task has a reference to the worker, then the worker may never be collected. If this happens, a memory leak could occur if we lose every other reference to the worker, since the worker nor the process will ever be collected.

We avoid this problem by only sharing the task queue. Then, the process does not have a strong reference to the worker and:
  - the worker can be garbage collected
  - if the worker (and all its associated futures) are garbage collected, since the worker process is unreachable we can schedule a #stop task. See method TKTWorker>> #finalize.