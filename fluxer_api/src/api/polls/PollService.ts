// SPDX-License-Identifier: AGPL-3.0-or-later

/**
 * PollService — wraps Rust backend poll operations.
 *
 * Methods delegate to the internal RPC layer which calls
 * fluxer_messages/src/polls.rs handlers on the Rust side.
 */

export interface CreatePollInput {
  channel_id: string;
  author_id: string;
  poll_data: {
    poll_id: string;
    title: string;
    options: Array<{
      index: number;
      text: string;
      image_url?: string;
      vote_count: number;
    }>;
    allow_multiple: boolean;
    is_anonymous: boolean;
    allow_custom_answers: boolean;
    expires_at?: number;
    is_closed: boolean;
  };
}

export interface CastVoteInput {
  poll_message_id: string;
  channel_id: string;
  user_id: string;
  option_indices: number[];
}

export interface GetResultsInput {
  poll_message_id: string;
  channel_id: string;
  requesting_user_id: string;
}

export interface ClosePollInput {
  poll_message_id: string;
  channel_id: string;
  user_id: string;
}

export interface PollResults {
  poll_id: string;
  title: string;
  options: Array<{
    index: number;
    text: string;
    vote_count: number;
    voters?: Array<{
      id: string;
      username: string;
      discriminator: string;
      global_name?: string;
      avatar?: string;
    }>;
  }>;
  total_votes: number;
  is_closed: boolean;
  is_anonymous: boolean;
}

export class PollService {
  constructor(private readonly rpcService: { call: (method: string, params: unknown) => Promise<unknown> }) {}

  async castVote(input: CastVoteInput): Promise<void> {
    await this.rpcService.call('polls.castVote', input);
  }

  async getResults(input: GetResultsInput): Promise<PollResults> {
    return this.rpcService.call('polls.getResults', input) as Promise<PollResults>;
  }

  async closePoll(input: ClosePollInput): Promise<void> {
    await this.rpcService.call('polls.closePoll', input);
  }
}
