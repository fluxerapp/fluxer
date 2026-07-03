// SPDX-License-Identifier: AGPL-3.0-or-later

import { Hono } from 'hono';
import { z } from 'zod';
import type { HonoApp, HonoEnv } from '../types/HonoEnv';
import { requireAuth } from '../middleware/AuthMiddleware';
import { validateBody } from '../middleware/ValidationMiddleware';

// --- Validation Schemas ---

const CreatePollSchema = z.object({
  title: z.string().min(1).max(256),
  options: z
    .array(
      z.object({
        text: z.string().min(1).max(512),
        image_url: z.string().url().optional(),
      }),
    )
    .min(2)
    .max(25),
  allow_multiple: z.boolean().default(false),
  is_anonymous: z.boolean().default(false),
  duration_minutes: z.number().int().min(1).max(10080).optional(),
  allow_custom_answers: z.boolean().default(false),
});

const VoteSchema = z.object({
  option_indices: z.array(z.number().int().min(0)).min(1),
});

// --- Route Registration ---

export function PollsController(app: HonoApp): void {
  const polls = new Hono<HonoEnv>();

  // POST /channels/:channelId/polls
  polls.post(
    '/channels/:channelId/polls',
    requireAuth,
    validateBody(CreatePollSchema),
    async (ctx) => {
      const channelId = ctx.req.param('channelId');
      const userId = ctx.get('user').id;
      const body = ctx.req.valid('json');

      const pollData = {
        poll_id: '', // assigned by snowflake service
        title: body.title,
        options: body.options.map((opt: any, i: number) => ({
          index: i,
          text: opt.text,
          image_url: opt.image_url,
          vote_count: 0,
        })),
        allow_multiple: body.allow_multiple,
        is_anonymous: body.is_anonymous,
        allow_custom_answers: body.allow_custom_answers,
        expires_at: body.duration_minutes
          ? Date.now() + body.duration_minutes * 60 * 1000
          : undefined,
        is_closed: false,
      };

      const message = await ctx.var.messageRequestService.createPollMessage({
        channel_id: channelId,
        author_id: userId,
        poll_data: pollData,
      });

      return ctx.json(message, 201);
    },
  );

  // POST /channels/:channelId/polls/:messageId/vote
  polls.post(
    '/channels/:channelId/polls/:messageId/vote',
    requireAuth,
    validateBody(VoteSchema),
    async (ctx) => {
      const channelId = ctx.req.param('channelId');
      const messageId = ctx.req.param('messageId');
      const userId = ctx.get('user').id;
      const { option_indices } = ctx.req.valid('json');

      await ctx.var.pollsService.castVote({
        poll_message_id: messageId,
        channel_id: channelId,
        user_id: userId,
        option_indices,
      });

      return ctx.json({ success: true });
    },
  );

  // GET /channels/:channelId/polls/:messageId/results
  polls.get(
    '/channels/:channelId/polls/:messageId/results',
    requireAuth,
    async (ctx) => {
      const channelId = ctx.req.param('channelId');
      const messageId = ctx.req.param('messageId');
      const userId = ctx.get('user').id;

      const results = await ctx.var.pollsService.getResults({
        poll_message_id: messageId,
        channel_id: channelId,
        requesting_user_id: userId,
      });

      return ctx.json(results);
    },
  );

  // DELETE /channels/:channelId/polls/:messageId
  polls.delete(
    '/channels/:channelId/polls/:messageId',
    requireAuth,
    async (ctx) => {
      const channelId = ctx.req.param('channelId');
      const messageId = ctx.req.param('messageId');
      const userId = ctx.get('user').id;

      await ctx.var.pollsService.closePoll({
        poll_message_id: messageId,
        channel_id: channelId,
        user_id: userId,
      });

      return ctx.json({ success: true });
    },
  );

  app.route('/', polls);
}
