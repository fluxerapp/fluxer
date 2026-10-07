// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

const REPO_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..', '..', '..', '..');
const SCANNED_ROOTS = ['fluxer_api/src', 'fluxer_app/src', 'packages'];
const SET_USE = /\b(GUILD_TEXT_BASED_CHANNEL_TYPES|TEXT_BASED_CHANNEL_TYPES)\b/g;
const IMPORT_STATEMENT = /^import\s[\s\S]*?from\s+'[^']+';$/gm;

const UNCHANGED = 'unchanged: threads and forums never reach this site or are handled by their own path';
const DEFINITION = 'definition: the sets never gain 11/12/15/16 (R13)';
const API_45 = 'API-45: threads resolve NSFW through resolveEffectiveThreadNsfw';
const THREAD_TYPES_SEPARATE = 'thread types accepted separately through THREAD_CHANNEL_TYPES, 15/16 stay rejected';
const WEBHOOK_TARGETS = 'webhook targets for 15/16 are resolved by API-8/9, threads are never webhook owners';
const HARVEST = 'WK-6: harvest adds THREAD_CHANNEL_TYPES next to the set';
const UPLOAD_TARGETS =
	'API-10: uploads accept THREAD_FEATURE_CHANNEL_TYPES next to the set, reached only past the viewer gate';
const FORUM_RATE_LIMIT = 'M8: forum and media rate limits are accepted next to the set';

const EXPECTED_SITES: Readonly<Record<string, {count: number; decision: string}>> = {
	'fluxer_api/src/api/channel/services/AttachmentUploadService.ts TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: UPLOAD_TARGETS,
	},
	'fluxer_api/src/api/channel/services/channel_data/ChannelOperationsService.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: FORUM_RATE_LIMIT,
	},
	'fluxer_api/src/api/channel/services/interaction/MessageInteractionBase.ts TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: THREAD_TYPES_SEPARATE,
	},
	'fluxer_api/src/api/channel/services/message/MessageContentService.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: API_45,
	},
	'fluxer_api/src/api/channel/services/message/MessageValidationService.ts TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: THREAD_TYPES_SEPARATE,
	},
	'fluxer_api/src/api/webhook/WebhookService.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {count: 1, decision: WEBHOOK_TARGETS},
	'fluxer_api/src/api/worker/tasks/HarvestGuildData.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {count: 1, decision: HARVEST},
	'fluxer_app/src/features/app/components/dialogs/components/plutonium/hooks/useCommunityActions.tsx GUILD_TEXT_BASED_CHANNEL_TYPES':
		{count: 1, decision: UNCHANGED},
	'fluxer_app/src/features/app/components/layout/GuildLayout.tsx GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: UNCHANGED,
	},
	'fluxer_app/src/features/channel/components/message_search_bar/useMessageSearchAutocomplete.ts GUILD_TEXT_BASED_CHANNEL_TYPES':
		{count: 1, decision: UNCHANGED},
	'fluxer_app/src/features/channel/components/modals/channel_tabs/ChannelOverviewTab.tsx GUILD_TEXT_BASED_CHANNEL_TYPES':
		{
			count: 2,
			decision: UNCHANGED,
		},
	'fluxer_app/src/features/channel/components/modals/channel_tabs/ChannelWebhooksTab.tsx GUILD_TEXT_BASED_CHANNEL_TYPES':
		{
			count: 2,
			decision: WEBHOOK_TARGETS,
		},
	'fluxer_app/src/features/channel/models/Channel.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {count: 1, decision: UNCHANGED},
	'fluxer_app/src/features/channel/state/Channels.ts TEXT_BASED_CHANNEL_TYPES': {count: 2, decision: UNCHANGED},
	'fluxer_app/src/features/channel/utils/ChannelCreateModalUtils.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 2,
		decision: UNCHANGED,
	},
	'fluxer_app/src/features/gateway/snapshot/SnapshotChannelReducer.ts TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: THREAD_TYPES_SEPARATE,
	},
	'fluxer_app/src/features/guild/components/modals/guild_tabs/GuildWebhooksTab.tsx GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: WEBHOOK_TARGETS,
	},
	'fluxer_app/src/features/guild/state/GuildReadState.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 2,
		decision: UNCHANGED,
	},
	'fluxer_app/src/features/invite/utils/InviteUtils.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {count: 2, decision: UNCHANGED},
	'fluxer_app/src/features/messaging/utils/ChannelShared.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: UNCHANGED,
	},
	'fluxer_app/src/features/read_state/state/ReadStates.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 2,
		decision: UNCHANGED,
	},
	'fluxer_app/src/features/read_state/state/ReadStates.ts TEXT_BASED_CHANNEL_TYPES': {count: 3, decision: UNCHANGED},
	'fluxer_app/src/features/search/components/search/ChannelFilterSheet.tsx GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: UNCHANGED,
	},
	'fluxer_app/src/features/ui/action_menu/ChannelContextMenu.tsx GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: UNCHANGED,
	},
	'fluxer_app/src/features/ui/action_menu/items/ChannelMenuItems.tsx GUILD_TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: UNCHANGED,
	},
	'fluxer_app/src/features/ui/action_menu/items/VoiceParticipantMenuData.tsx TEXT_BASED_CHANNEL_TYPES': {
		count: 1,
		decision: UNCHANGED,
	},
	'packages/constants/src/ChannelConstants.ts GUILD_TEXT_BASED_CHANNEL_TYPES': {count: 2, decision: DEFINITION},
	'packages/constants/src/ChannelConstants.ts TEXT_BASED_CHANNEL_TYPES': {count: 1, decision: DEFINITION},
};

function listSourceFiles(directory: string): Array<string> {
	return fs.readdirSync(directory, {withFileTypes: true}).flatMap((entry) => {
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			return entry.name === 'node_modules' || entry.name === 'dist' || entry.name === 'tests'
				? []
				: listSourceFiles(resolved);
		}
		if (entry.name.endsWith('.test.ts') || entry.name.endsWith('.test.tsx')) return [];
		return entry.name.endsWith('.ts') || entry.name.endsWith('.tsx') ? [resolved] : [];
	});
}

function collectSites(): Record<string, number> {
	const counts: Record<string, number> = {};
	for (const root of SCANNED_ROOTS) {
		for (const file of listSourceFiles(path.join(REPO_ROOT, root))) {
			const source = fs.readFileSync(file, 'utf8').replace(IMPORT_STATEMENT, '');
			const relative = path.relative(REPO_ROOT, file).split(path.sep).join('/');
			for (const match of source.matchAll(SET_USE)) {
				const key = `${relative} ${match[1]}`;
				counts[key] = (counts[key] ?? 0) + 1;
			}
		}
	}
	return counts;
}

describe('text type set consumers', () => {
	it('lists every consumer of the text type sets with a decision', () => {
		const expected = Object.fromEntries(Object.entries(EXPECTED_SITES).map(([key, site]) => [key, site.count]));
		expect(collectSites()).toEqual(expected);
	});
});
