// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';

export type AuditLogTone = 'add' | 'remove' | 'neutral';

export type AuditLogPlaceholder =
	| {kind: 'user'; id: string}
	| {kind: 'system'}
	| {kind: 'channel'; id: string; recordedName: string | null; fallback: 'channel' | 'category'}
	| {kind: 'role'; id: string; recordedName: string | null}
	| {kind: 'name'; value: string}
	| {kind: 'text'; value: string}
	| {kind: 'emoji'; id: string | null; name: string}
	| {kind: 'date'; timestamp: number}
	| {kind: 'duration'; seconds: number}
	| {kind: 'permissions'; flags: Array<bigint>}
	| {kind: 'color'; value: number}
	| {kind: 'label'; descriptor: MessageDescriptor};

export interface AuditLogSentence {
	descriptor: MessageDescriptor;
	values: Record<string, AuditLogPlaceholder | number>;
}

export interface AuditLogDetailRow {
	id: string;
	tone: AuditLogTone;
	sentence: AuditLogSentence;
}

export interface AuditLogBlock {
	kind: 'reason' | 'ban_reason';
	text: string;
}

export interface AuditLogPresentationContext {
	guildId: string;
	getWebhookName: (id: string) => string | null;
	getEmojiName: (id: string) => string | null;
	getStickerName: (id: string) => string | null;
}

export interface AuditLogDomainResult {
	summary: AuditLogSentence;
	rows: Array<AuditLogDetailRow>;
	blocks: Array<AuditLogBlock>;
}

export interface AuditLogEntryPresentation extends AuditLogDomainResult {
	expandable: boolean;
}

export type AuditLogPresenter = (
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
) => AuditLogDomainResult;
