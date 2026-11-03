// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {resolveLimit, resolveLimits} from '@fluxer/limits/src/LimitResolver';
import type {LimitConfigSnapshot, LimitMatchContext} from '@fluxer/limits/src/LimitTypes';
import {describe, expect, test} from 'vitest';

describe('LimitResolver', () => {
	test('resolveLimits returns default free limits for empty snapshot', () => {
		const snapshot: LimitConfigSnapshot = {
			traitDefinitions: [],
			rules: [],
		};
		const ctx: LimitMatchContext = {
			traits: new Set(),
			guildFeatures: new Set(),
		};
		const result = resolveLimits(snapshot, ctx);
		expect(result.limits).toEqual(DEFAULT_RESTRICTED_LIMITS);
	});
	test('resolveLimits applies premium limits for premium trait', () => {
		const snapshot: LimitConfigSnapshot = {
			traitDefinitions: ['premium'],
			rules: [
				{
					id: 'default',
					limits: {...DEFAULT_RESTRICTED_LIMITS},
				},
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {...DEFAULT_STOCK_LIMITS},
				},
			],
		};
		const ctx: LimitMatchContext = {
			traits: new Set(['premium']),
			guildFeatures: new Set(),
		};
		const result = resolveLimits(snapshot, ctx);
		expect(result.limits.max_guilds).toBe(200);
		expect(result.limits.max_message_length).toBe(4000);
		expect(result.limits.feature_animated_avatar).toBe(1);
	});
	test('resolveLimits merges with Math.max', () => {
		const snapshot: LimitConfigSnapshot = {
			traitDefinitions: ['premium', 'special'],
			rules: [
				{
					id: 'default',
					limits: {max_guilds: 100},
				},
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {max_guilds: 200},
				},
				{
					id: 'special',
					filters: {traits: ['special']},
					limits: {max_guilds: 150},
				},
			],
		};
		const ctx: LimitMatchContext = {
			traits: new Set(['premium', 'special']),
			guildFeatures: new Set(),
		};
		const result = resolveLimits(snapshot, ctx);
		expect(result.limits.max_guilds).toBe(200);
	});
	test('resolveLimits lets default rules override stock fallback values', () => {
		const snapshot: LimitConfigSnapshot = {
			traitDefinitions: [],
			rules: [
				{
					id: 'default',
					limits: {
						max_guilds: 50,
						max_message_length: 1000,
					},
				},
			],
		};
		const ctx: LimitMatchContext = {
			traits: new Set(),
			guildFeatures: new Set(),
		};
		const result = resolveLimits(snapshot, ctx);
		expect(result.limits.max_guilds).toBe(50);
		expect(result.limits.max_message_length).toBe(1000);
	});
	test('resolveLimits applies rules by specificity order', () => {
		const snapshot: LimitConfigSnapshot = {
			traitDefinitions: ['premium'],
			rules: [
				{
					id: 'combined',
					filters: {traits: ['premium'], guildFeatures: ['MORE_EMOJI']},
					limits: {max_guild_emojis: 500},
				},
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {max_guild_emojis: 100},
				},
				{
					id: 'default',
					limits: {max_guild_emojis: 50},
				},
			],
		};
		const ctx: LimitMatchContext = {
			traits: new Set(['premium']),
			guildFeatures: new Set(['MORE_EMOJI']),
		};
		const result = resolveLimits(snapshot, ctx, {evaluationContext: 'guild'});
		expect(result.limits.max_guild_emojis).toBe(500);
	});
	test('resolveLimit returns specific limit value', () => {
		const snapshot: LimitConfigSnapshot = {
			traitDefinitions: ['premium'],
			rules: [
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {max_guilds: 200},
				},
			],
		};
		const ctx: LimitMatchContext = {
			traits: new Set(['premium']),
			guildFeatures: new Set(),
		};
		const result = resolveLimit(snapshot, ctx, 'max_guilds');
		expect(result).toBe(200);
	});
	test('resolveLimits ignores non-matching rules', () => {
		const snapshot: LimitConfigSnapshot = {
			traitDefinitions: ['premium', 'special'],
			rules: [
				{
					id: 'default',
					limits: {max_guilds: 100},
				},
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {max_guilds: 200},
				},
				{
					id: 'special',
					filters: {traits: ['special']},
					limits: {max_guilds: 300},
				},
			],
		};
		const ctx: LimitMatchContext = {
			traits: new Set(['premium']),
			guildFeatures: new Set(),
		};
		const result = resolveLimits(snapshot, ctx);
		expect(result.limits.max_guilds).toBe(200);
	});
});
