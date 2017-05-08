Additionally, TaskIt provides an alternative means to create services through blocks (or valuables actually) using `TKTParameterizableService`. An alternative implementation of the file watcher could be done as follows.

```smalltalk
service := TKTParameterizableService new.
service name: 'Generic watcher service'.
service onSetUpDo: [ Transcript show: 'File watcher started' ].
service onTearDownDo: [ Transcript show: 'File watcher finished' ].
service step: [
  'temp.txt' asFileReference exists
    ifFalse: [ Transcript show: 'file does not exist!' ] ].

service start.
```