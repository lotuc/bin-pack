# Testing

Excludes the large data set testing when is under developing.

```
clj -X:test :dirs '["envs/test/"]' :excludes '[:find-best-pack-large-input]'
```

Test the `lotuc.binpack.eb-afit/find-best-pack` on a specific file using the
`:find-best` test and environment variable `input_file`:

```sh
# with resources
input_file=3d-bin-pack-test/mpp02.txt \
    clj -X:test :dirs '["envs/test/"]' :includes '[:find-best-pack]'

# with file
input_file=/path/to/your-input.txt \
    clj -X:test :dirs '["envs/test/"]' :includes '[:find-best-pack]'
```
