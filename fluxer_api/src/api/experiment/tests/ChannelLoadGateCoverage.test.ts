// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

const API_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
const DIRECT_LOAD =
	/(?:channelRepository|ChannelRepository\(\)|ChannelDataRepository\(\)|channelRepo|channelDataRepository|channelData|get\('channelRepository'\))\.findUnique\(/g;
const GATE_REFERENCE = /ChannelThreadsGate|\bisThread\w*\(|THREAD_\w*CHANNEL_TYPES/;

type Coverage = 'gated' | 'dm_or_voice_only' | 'admin_or_system';

const EXPECTED_LOADS: Readonly<Record<string, readonly [number, Coverage]>> = {
	'admin/controllers/ThreadAdminController.ts': [1, 'admin_or_system'],
	'admin/services/AdminAuditService.ts': [1, 'admin_or_system'],
	'admin/services/AdminMessageService.ts': [4, 'admin_or_system'],
	'admin/services/AdminReportService.ts': [4, 'gated'],
	'channel/ChannelRepository.ts': [1, 'admin_or_system'],
	'channel/services/AttachmentUploadService.ts': [1, 'gated'],
	'channel/services/BaseChannelAuthService.ts': [4, 'gated'],
	'channel/services/CallService.ts': [5, 'dm_or_voice_only'],
	'channel/services/PersonalNotesChannelRepair.ts': [1, 'dm_or_voice_only'],
	'channel/services/channel_data/ChannelOperationsService.ts': [5, 'gated'],
	'channel/services/channel_data/GroupDmUpdateService.ts': [1, 'dm_or_voice_only'],
	'channel/services/group_dm/GroupDmOperationsService.ts': [4, 'dm_or_voice_only'],
	'channel/services/message/CrosspostDeliveryService.ts': [8, 'admin_or_system'],
	'channel/services/message/MessageDeleteService.ts': [1, 'gated'],
	'channel/services/message/MessagePersistenceService.ts': [1, 'admin_or_system'],
	'channel/services/message/MessageProcessingService.ts': [1, 'admin_or_system'],
	'channel/services/message/MessageRequestParser.ts': [1, 'gated'],
	'channel/services/message/MessageRetrievalService.ts': [1, 'gated'],
	'channel/services/message/MessageSendService.ts': [2, 'gated'],
	'channel/services/message/MessageSystemService.ts': [1, 'admin_or_system'],
	'channel/services/message/ThreadMessageActivity.ts': [1, 'admin_or_system'],
	'channel/services/message/UserMessageDeletionService.ts': [2, 'admin_or_system'],
	'channel/services/thread/ThreadCreationService.ts': [4, 'gated'],
	'channel/services/thread/ThreadDeletionService.ts': [1, 'admin_or_system'],
	'csam/NcmecSubmissionService.ts': [1, 'admin_or_system'],
	'guild/services/channel/ChannelOperationsService.ts': [2, 'gated'],
	'guild/services/GuildDiscoveryService.ts': [1, 'gated'],
	'guild/services/data/GuildOperationsService.ts': [2, 'gated'],
	'middleware/GuildAvailabilityMiddleware.ts': [1, 'admin_or_system'],
	'report/ReportService.ts': [3, 'gated'],
	'rpc/RpcService.ts': [3, 'gated'],
	'search/GlobalSearchService.ts': [3, 'gated'],
	'search/MessageSearchResponseMapper.ts': [1, 'gated'],
	'user/entrance_sound/EntranceSoundPlayService.ts': [1, 'dm_or_voice_only'],
	'voice/VoiceService.ts': [1, 'dm_or_voice_only'],
	'webhook/ChannelFollowService.ts': [2, 'gated'],
	'webhook/WebhookModel.ts': [1, 'admin_or_system'],
	'webhook/WebhookRequestService.ts': [1, 'gated'],
	'webhook/WebhookService.ts': [7, 'gated'],
	'worker/tasks/ExtractEmbeds.ts': [1, 'admin_or_system'],
	'worker/tasks/HandleMentions.ts': [1, 'admin_or_system'],
	'worker/tasks/HarvestUserData.ts': [1, 'gated'],
	'worker/tasks/IndexChannelMessages.ts': [1, 'admin_or_system'],
	'worker/tasks/RemoveChannelFollowers.ts': [1, 'admin_or_system'],
	'worker/tasks/ThreadMentionScope.ts': [1, 'gated'],
	'worker/tasks/utils/MessageDeletion.ts': [1, 'admin_or_system'],
};

function listSourceFiles(directory: string): Array<string> {
	return fs.readdirSync(directory, {withFileTypes: true}).flatMap((entry) => {
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) return entry.name === 'tests' ? [] : listSourceFiles(resolved);
		return entry.name.endsWith('.ts') && !entry.name.endsWith('.test.ts') ? [resolved] : [];
	});
}

function collectLoads(): Map<string, {count: number; source: string}> {
	const loads = new Map<string, {count: number; source: string}>();
	for (const file of listSourceFiles(API_ROOT)) {
		const relative = path.relative(API_ROOT, file).split(path.sep).join('/');
		if (relative.startsWith('test/')) continue;
		const source = fs.readFileSync(file, 'utf8');
		const count = source.match(DIRECT_LOAD)?.length ?? 0;
		if (count > 0) loads.set(relative, {count, source});
	}
	return loads;
}

describe('direct channel load gate coverage', () => {
	const loads = collectLoads();

	it('finds exactly the allowlisted direct channel loads', () => {
		const counts = Object.fromEntries([...loads].map(([file, {count}]) => [file, count]));
		const expected = Object.fromEntries(Object.entries(EXPECTED_LOADS).map(([file, [count]]) => [file, count]));
		expect(counts).toEqual(expected);
	});

	it('references the thread gate in every file marked gated', () => {
		const ungated = Object.entries(EXPECTED_LOADS)
			.filter(([file, [, coverage]]) => coverage === 'gated' && !GATE_REFERENCE.test(loads.get(file)?.source ?? ''))
			.map(([file]) => file);
		expect(ungated).toEqual([]);
	});
});
