-- Poll Votes Table
-- Stores individual votes for poll options
CREATE TABLE IF NOT EXISTS fluxer.poll_votes (
    poll_id bigint,
    option_index int,
    user_id bigint,
    voted_at timestamp,
    PRIMARY KEY (poll_id, option_index, user_id)
) WITH compaction = {'class': 'LeveledCompactionStrategy'};

-- Poll Messages Index (for looking up polls by channel)
-- Polls are stored as regular messages with embed_type = 'poll'
-- This table enables efficient poll-specific queries
CREATE TABLE IF NOT EXISTS fluxer.poll_messages (
    channel_id bigint,
    message_id bigint,
    poll_data text,  -- JSON-serialized PollData
    created_at timestamp,
    expires_at timestamp,
    is_closed boolean,
    PRIMARY KEY (channel_id, message_id)
) WITH compaction = {'class': 'LeveledCompactionStrategy'};

-- Custom Poll Answers (user-submitted options)
CREATE TABLE IF NOT EXISTS fluxer.poll_custom_answers (
    poll_id bigint,
    answer_index int,
    user_id bigint,
    text text,
    created_at timestamp,
    PRIMARY KEY (poll_id, answer_index, user_id)
);
