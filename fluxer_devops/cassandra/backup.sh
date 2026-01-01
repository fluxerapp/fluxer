#!/bin/bash

# Copyright (C) 2026 Fluxer Contributors
#
# This file is part of Fluxer.
#
# Fluxer is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Fluxer is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

set -euo pipefail

TIMESTAMP=$(date +%Y%m%d-%H%M%S)
BACKUP_NAME="cassandra-backup-${TIMESTAMP}"
SNAPSHOT_TAG="backup-${TIMESTAMP}"
DATA_DIR="/var/lib/cassandra/data"
TEMP_DIR="/tmp/${BACKUP_NAME}"
AGE_PUBLIC_KEY_FILE="${AGE_PUBLIC_KEY_FILE:-/tmp/age_public_key.txt}"
ENCRYPTED_BACKUP="${BACKUP_NAME}.tar.age"
MAX_BACKUP_COUNT=168  # 7 days of hourly backups
CASSANDRA_HOST="${CASSANDRA_HOST:-cassandra}"

# AWS CLI configuration for B2
export AWS_ACCESS_KEY_ID="${B2_KEY_ID}"
export AWS_SECRET_ACCESS_KEY="${B2_APPLICATION_KEY}"
export AWS_DEFAULT_REGION="${B2_REGION}"
B2_ENDPOINT_URL="https://${B2_ENDPOINT}"

echo "[$(date)] Starting Cassandra backup: ${BACKUP_NAME}"

# Step 1: Create snapshot
echo "[$(date)] Creating Cassandra snapshot: ${SNAPSHOT_TAG}"
if ! nodetool -h "${CASSANDRA_HOST}" snapshot -t "${SNAPSHOT_TAG}"; then
    echo "[$(date)] Error: Failed to create snapshot"
    exit 1
fi
echo "[$(date)] Snapshot created successfully"

# Step 2: Collect snapshot files
echo "[$(date)] Collecting snapshot files"
mkdir -p "${TEMP_DIR}"

# Find all snapshot directories and copy to temp location
find "${DATA_DIR}" -type d -name "${SNAPSHOT_TAG}" | while read snapshot_dir; do
    # Get relative path from data dir
    rel_path=$(dirname $(echo "${snapshot_dir}" | sed "s|${DATA_DIR}/||"))
    target_dir="${TEMP_DIR}/${rel_path}"
    mkdir -p "${target_dir}"
    cp -r "${snapshot_dir}" "${target_dir}/"
done

# Copy schema
echo "[$(date)] Saving schema"
if [ -n "${CASSANDRA_PASSWORD:-}" ]; then
    if ! cqlsh -u cassandra -p "${CASSANDRA_PASSWORD}" "${CASSANDRA_HOST}" -e "DESC SCHEMA;" 2>/dev/null | sed '/^WARNING:/d' > "${TEMP_DIR}/schema.cql"; then
        echo "Warning: Could not export schema"
    fi
else
    if ! cqlsh "${CASSANDRA_HOST}" -e "DESC SCHEMA;" 2>/dev/null | sed '/^WARNING:/d' > "${TEMP_DIR}/schema.cql"; then
        echo "Warning: Could not export schema (no password set)"
    fi
fi

# Save cluster topology info
nodetool -h "${CASSANDRA_HOST}" describecluster > "${TEMP_DIR}/cluster_topology.txt" 2>/dev/null || true
nodetool -h "${CASSANDRA_HOST}" status > "${TEMP_DIR}/cluster_status.txt" 2>/dev/null || true

echo "[$(date)] Snapshot collection completed"

# Step 3: Check if encryption is enabled
if [ ! -f "${AGE_PUBLIC_KEY_FILE}" ]; then
    echo "[$(date)] Warning: Age public key not found - skipping encryption and upload"
    echo "[$(date)] Backup stored locally at: ${TEMP_DIR}"

    # Clear snapshot from Cassandra
    nodetool -h "${CASSANDRA_HOST}" clearsnapshot -t "${SNAPSHOT_TAG}"
    exit 0
fi

# Step 4: Create tar archive and encrypt with age (streaming)
echo "[$(date)] Encrypting backup with age..."
if ! tar -C /tmp -cf - "${BACKUP_NAME}" | \
     age -r "$(cat ${AGE_PUBLIC_KEY_FILE})" -o "/tmp/${ENCRYPTED_BACKUP}"; then
    echo "[$(date)] Error: Encryption failed"
    rm -rf "${TEMP_DIR}"
    nodetool -h "${CASSANDRA_HOST}" clearsnapshot -t "${SNAPSHOT_TAG}"
    exit 1
fi
echo "[$(date)] Encryption completed: ${ENCRYPTED_BACKUP}"

# Get file size
BACKUP_SIZE=$(du -h "/tmp/${ENCRYPTED_BACKUP}" | cut -f1)
echo "[$(date)] Encrypted backup size: ${BACKUP_SIZE}"

# Step 5: Upload encrypted backup to B2
echo "[$(date)] Uploading encrypted backup to B2..."
if ! aws s3 cp "/tmp/${ENCRYPTED_BACKUP}" \
     "s3://${B2_BUCKET_NAME}/${ENCRYPTED_BACKUP}" \
     --endpoint-url="${B2_ENDPOINT_URL}"; then
    echo "[$(date)] Error: Upload to B2 failed"
    rm -f "/tmp/${ENCRYPTED_BACKUP}"
    rm -rf "${TEMP_DIR}"
    nodetool -h "${CASSANDRA_HOST}" clearsnapshot -t "${SNAPSHOT_TAG}"
    exit 1
fi
echo "[$(date)] Upload completed successfully"

# Step 6: Cleanup
echo "[$(date)] Cleaning up temporary files..."
rm -f "/tmp/${ENCRYPTED_BACKUP}"
rm -rf "${TEMP_DIR}"

# Clear snapshot from Cassandra
echo "[$(date)] Clearing snapshot from Cassandra"
nodetool -h "${CASSANDRA_HOST}" clearsnapshot -t "${SNAPSHOT_TAG}"

# Step 7: Purge old backups from B2
echo "[$(date)] Purging old backups from B2 (keeping last ${MAX_BACKUP_COUNT})..."
aws s3 ls "s3://${B2_BUCKET_NAME}/" --endpoint-url="${B2_ENDPOINT_URL}" | \
    grep "cassandra-backup-.*\.tar\.age$" | \
    awk '{print $4}' | \
    sort -r | \
    tail -n +$((MAX_BACKUP_COUNT + 1)) | \
    while read -r old_backup; do
        echo "[$(date)] Deleting old backup: ${old_backup}"
        aws s3 rm "s3://${B2_BUCKET_NAME}/${old_backup}" --endpoint-url="${B2_ENDPOINT_URL}" || true
    done

echo "[$(date)] Backup process completed successfully"
echo "[$(date)] Backup name: ${ENCRYPTED_BACKUP}"
echo "[$(date)] Backup size: ${BACKUP_SIZE}"