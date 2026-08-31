// SPDX-License-Identifier: AGPL-3.0-or-later

import z from "zod";

const MessagePollEmojiSchema = z.object({
    id: z.string().nullish().describe('The ID of the emoji'),
    name: z.string().nullish().describe('The name of the emoji'),
});

const MessagePollMediaSchema = z.object({
    emoji: MessagePollEmojiSchema.nullish().describe('The emoji of the poll media'),
    text: z.string().nullish().describe('The text of the poll media'),
});

const MessagePollAnswerSchema = z.object({
    answer_id: z.int32().nullish().describe('The ID of the poll answer (starts at 1)'),
    poll_media: MessagePollMediaSchema.nullish().describe('The poll media of the answer (contains the text and emoji)'),
});

const MessagePollAnswerCountSchema = z.object({
    id: z.int32().nullish().describe('The ID of the poll answer'),
    count: z.int32().nullish().describe('The number of votes on this answer'),
    me_voted: z.boolean().nullish().describe('Whether the requesting user voted on this answer'),
});

const MessagePollResultsSchema = z.object({
    answer_counts: z.array(MessagePollAnswerCountSchema).nullish().describe('The answer counts of the poll results'),
    is_finalized: z.boolean().nullish().describe('Whether the poll results are finalized'),
});

export const MessagePollResponse = z.object({
    question: MessagePollMediaSchema.nullish().describe('The question of the poll'),
    answers: z.array(MessagePollAnswerSchema).nullish().describe('The possible answers of the poll'),
    expiry: z.iso.datetime().nullish().describe('Expiration date of the poll as an ISO date'),
    anonymous_voting: z.boolean().nullish().describe('Whether the votes are anonymous on this poll'),
    allow_multiselect: z.boolean().nullish().describe('Whether the poll allows multiple answers'),
    layout_type: z.int32().nullish().describe('The layout type of the poll'),
    results: MessagePollResultsSchema.nullish().describe('The results of the poll'),
});
export type MessagePollResponse = z.infer<typeof MessagePollResponse>;

export const MessagePollRequest = z.object({
    question: MessagePollMediaSchema.nullish().describe('The question of the poll'),
    answers: z.array(MessagePollAnswerSchema).nullish().describe('The possible answers of the poll'),
    duration: z.int32().nullish().describe('Duration of the poll in hours'),
    anonymous_voting: z.boolean().nullish().describe('Whether the votes are anonymous on this poll'),
    allow_multiselect: z.boolean().nullish().describe('Whether the poll allows multiple answers'),
    layout_type: z.int32().nullish().describe('The layout type of the poll'),
});
export type MessagePollRequest = z.infer<typeof MessagePollRequest>;

export interface MessagePollEmoji {
    id?: string;
    name?: string;
}

export interface MessagePollMedia {
    emoji?: MessagePollEmoji;
    text?: string;
}

export interface MessagePollAnswer {
    answer_id?: number;
    poll_media?: MessagePollMedia;
}

export interface MessagePollAnswerCount {
    id?: number;
    count?: number;
    me_voted?: boolean;
}

export interface MessagePollResults {
    answer_counts?: Array<MessagePollAnswerCount>;
    is_finalized?: boolean;
}

export interface MessageCreatePoll {
    question?: MessagePollMedia;
    answers?: Array<MessagePollAnswer>;
    duration?: number;
    anonymous_voting?: boolean;
    allow_multiselect?: boolean;
    layout_type?: number;
    results?: MessagePollResults;
}

export interface MessagePoll {
    question?: MessagePollMedia;
    answers?: Array<MessagePollAnswer>;
    expiry?: string;
    anonymous_voting?: boolean;
    allow_multiselect?: boolean;
    layout_type?: number;
    results?: MessagePollResults;
}
