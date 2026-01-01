CREATE TABLE IF NOT EXISTS fluxer.admin_archives_by_subject (
    subject_type text,
    subject_id bigint,
    archive_id bigint,
    requested_by bigint,
    requested_at timestamp,
    started_at timestamp,
    completed_at timestamp,
    failed_at timestamp,
    storage_key text,
    file_size bigint,
    progress_percent int,
    progress_step text,
    error_message text,
    download_url_expires_at timestamp,
    expires_at timestamp,
    PRIMARY KEY ((subject_type, subject_id), archive_id)
) WITH CLUSTERING ORDER BY (archive_id DESC);

CREATE TABLE IF NOT EXISTS fluxer.admin_archives_by_requester (
    requested_by bigint,
    archive_id bigint,
    subject_type text,
    subject_id bigint,
    requested_at timestamp,
    started_at timestamp,
    completed_at timestamp,
    failed_at timestamp,
    storage_key text,
    file_size bigint,
    progress_percent int,
    progress_step text,
    error_message text,
    download_url_expires_at timestamp,
    expires_at timestamp,
    PRIMARY KEY ((requested_by), archive_id)
) WITH CLUSTERING ORDER BY (archive_id DESC);

CREATE TABLE IF NOT EXISTS fluxer.admin_archives_by_type (
    subject_type text,
    archive_id bigint,
    subject_id bigint,
    requested_by bigint,
    requested_at timestamp,
    started_at timestamp,
    completed_at timestamp,
    failed_at timestamp,
    storage_key text,
    file_size bigint,
    progress_percent int,
    progress_step text,
    error_message text,
    download_url_expires_at timestamp,
    expires_at timestamp,
    PRIMARY KEY ((subject_type), archive_id)
) WITH CLUSTERING ORDER BY (archive_id DESC);
