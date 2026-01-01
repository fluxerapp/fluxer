# Cassandra Restore

## Fresh Instance from Local Backup

```bash
# 1. Create volume and start Cassandra
docker volume create cassandra_data
docker run -d --name cass -v cassandra_data:/var/lib/cassandra -p 9042:9042 cassandra:5.0
echo "Waiting for Cassandra to start..."
sleep 30

# 2. Extract backup and apply schema
docker exec cass bash -c 'apt-get update -qq && apt-get install -y -qq age'
docker cp ~/Downloads/backup.tar.age cass:/tmp/
docker cp ~/Downloads/key.txt cass:/tmp/
docker exec cass bash -c 'age -d -i /tmp/key.txt /tmp/backup.tar.age | tar -C /tmp -xf -'
docker exec cass bash -c 'sed "/^WARNING:/d" /tmp/cassandra-backup-*/schema.cql | cqlsh'

# 3. Copy backup to volume and stop Cassandra
docker exec cass bash -c 'cp -r /tmp/cassandra-backup-* /var/lib/cassandra/'
docker stop cass
docker run -d --name cass-util -v cassandra_data:/var/lib/cassandra --entrypoint sleep cassandra:5.0 infinity
docker exec cass-util bash -c '
  BACKUP_DIR=$(ls -d /var/lib/cassandra/cassandra-backup-* | head -1)
  DATA_DIR=/var/lib/cassandra/data
  for keyspace_dir in "$BACKUP_DIR"/*/; do
    keyspace=$(basename "$keyspace_dir")
    [[ "$keyspace" =~ ^system ]] && continue
    [ ! -d "$keyspace_dir" ] && continue
    for snapshot_dir in "$keyspace_dir"/*/snapshots/backup-*/; do
      [ ! -d "$snapshot_dir" ] && continue
      table_with_uuid=$(basename $(dirname $(dirname "$snapshot_dir")))
      table_name=$(echo "$table_with_uuid" | cut -d- -f1)
      target_dir=$(ls -d "$DATA_DIR/$keyspace/${table_name}"-* 2>/dev/null | head -1)
      if [ -n "$target_dir" ]; then
        cp "$snapshot_dir"/* "$target_dir"/ 2>/dev/null || true
      fi
    done
  done
  chown -R cassandra:cassandra "$DATA_DIR"
'

# 4. Restart Cassandra and refresh tables
docker rm -f cass-util
docker start cass
sleep 30

# 5. Run nodetool refresh on all tables
docker exec cass bash -c '
  BACKUP_DIR=$(ls -d /var/lib/cassandra/cassandra-backup-* | head -1)
  for keyspace_dir in "$BACKUP_DIR"/*/; do
    keyspace=$(basename "$keyspace_dir")
    [[ "$keyspace" =~ ^system ]] && continue
    for snapshot_dir in "$keyspace_dir"/*/snapshots/backup-*/; do
      [ ! -d "$snapshot_dir" ] && continue
      table_with_uuid=$(basename $(dirname $(dirname "$snapshot_dir")))
      table_name=$(echo "$table_with_uuid" | cut -d- -f1)
      nodetool refresh -- "$keyspace" "$table_name" 2>&1 | grep -v deprecated || true
    done
  done
'

# 6. Verify
docker exec cass cqlsh -e "SELECT COUNT(*) FROM fluxer.users;"
```

## Production Restore from B2

> [!IMPORTANT]
> This assumes you have B2 credentials configured on the server.

```bash
# 0. Set variables
BACKUP_NAME="cassandra-backup-20251016-103753.tar.age"  # Replace with actual backup name
CASSANDRA_CONTAINER="cassandra-prod"

# 1. Download backup from B2 (on the server)
export AWS_ACCESS_KEY_ID="${B2_KEY_ID}"
export AWS_SECRET_ACCESS_KEY="${B2_APPLICATION_KEY}"
export AWS_DEFAULT_REGION="${B2_REGION}"
B2_ENDPOINT_URL="https://${B2_ENDPOINT}"

aws s3 cp "s3://${B2_BUCKET_NAME}/${BACKUP_NAME}" \
  "/tmp/${BACKUP_NAME}" \
  --endpoint-url="${B2_ENDPOINT_URL}"

# 2. Copy backup and key to Cassandra container
docker cp "/tmp/${BACKUP_NAME}" ${CASSANDRA_CONTAINER}:/tmp/
docker cp /etc/cassandra/age_private_key.txt ${CASSANDRA_CONTAINER}:/tmp/key.txt

# 3. Stop Cassandra and prepare
docker exec ${CASSANDRA_CONTAINER} bash -c 'apt-get update -qq && apt-get install -y -qq age'
docker stop ${CASSANDRA_CONTAINER}

# 4. Extract backup in utility container
docker run -d --name cass-restore-util --volumes-from ${CASSANDRA_CONTAINER} --entrypoint sleep cassandra:5.0 infinity
docker exec cass-restore-util bash -c 'age -d -i /tmp/key.txt /tmp/${BACKUP_NAME} | tar -C /tmp -xf -'
docker exec cass-restore-util bash -c 'cp -r /tmp/cassandra-backup-* /var/lib/cassandra/'

# 5. Copy SSTable files to existing schema directories
docker exec cass-restore-util bash -c '
  BACKUP_DIR=$(ls -d /var/lib/cassandra/cassandra-backup-* | head -1)
  DATA_DIR=/var/lib/cassandra/data
  for keyspace_dir in "$BACKUP_DIR"/*/; do
    keyspace=$(basename "$keyspace_dir")
    [[ "$keyspace" =~ ^system ]] && continue
    [ ! -d "$keyspace_dir" ] && continue
    for snapshot_dir in "$keyspace_dir"/*/snapshots/backup-*/; do
      [ ! -d "$snapshot_dir" ] && continue
      table_with_uuid=$(basename $(dirname $(dirname "$snapshot_dir")))
      table_name=$(echo "$table_with_uuid" | cut -d- -f1)
      target_dir=$(ls -d "$DATA_DIR/$keyspace/${table_name}"-* 2>/dev/null | head -1)
      if [ -n "$target_dir" ]; then
        cp "$snapshot_dir"/* "$target_dir"/ 2>/dev/null || true
      fi
    done
  done
  chown -R cassandra:cassandra "$DATA_DIR"
'

# 6. Restart Cassandra
docker rm -f cass-restore-util
docker start ${CASSANDRA_CONTAINER}
sleep 30

# 7. Run nodetool refresh
docker exec ${CASSANDRA_CONTAINER} bash -c '
  BACKUP_DIR=$(ls -d /var/lib/cassandra/cassandra-backup-* | head -1)
  for keyspace_dir in "$BACKUP_DIR"/*/; do
    keyspace=$(basename "$keyspace_dir")
    [[ "$keyspace" =~ ^system ]] && continue
    for snapshot_dir in "$keyspace_dir"/*/snapshots/backup-*/; do
      [ ! -d "$snapshot_dir" ] && continue
      table_with_uuid=$(basename $(dirname $(dirname "$snapshot_dir")))
      table_name=$(echo "$table_with_uuid" | cut -d- -f1)
      nodetool refresh -- "$keyspace" "$table_name" 2>&1 | grep -v deprecated || true
    done
  done
'

# 8. Verify
docker exec ${CASSANDRA_CONTAINER} cqlsh -e "SELECT COUNT(*) FROM fluxer.users;"

# 9. Cleanup
rm -f "/tmp/${BACKUP_NAME}"
```
