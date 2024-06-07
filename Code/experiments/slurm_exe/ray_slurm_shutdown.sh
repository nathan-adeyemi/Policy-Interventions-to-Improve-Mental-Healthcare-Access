nodes=$(scontrol show hostnames "$SLURM_JOB_NODELIST")
nodes_array=($nodes)

# Stop Ray on the head node
head_node=${nodes_array[0]}
echo "Stopping Ray on HEAD node: $head_node"
ssh "$head_node" "ray stop"

# Stop Ray on each worker node
worker_num=$((SLURM_JOB_NUM_NODES - 1))
for ((i = 1; i <= worker_num; i++)); do
    node_i=${nodes_array[$i]}
    echo "Stopping Ray on WORKER node: $node_i"
    ssh "$node_i" "ray stop"
done