// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

const requests: Array<string> = [];
const responses = new Map<string, () => unknown>();
let preupload = false;
let messageUpload: {attachments: Array<{id: number}>} | null = null;
let prepared: {attachments: Array<Record<string, unknown>>; files: Array<File>} = {attachments: [], files: []};

function record(method: string, url: string, options: {body?: unknown}): unknown {
	const body =
		options.body instanceof File
			? `<file ${options.body.name}>`
			: options.body instanceof Blob
				? `<blob ${options.body.size}>`
				: JSON.stringify(options.body);
	requests.push(`${method} ${url} ${body}`);
	const response = responses.get(`${method} ${url}`);
	return {body: response ? response() : undefined};
}

vi.stubGlobal('window', {setTimeout: () => 0});
vi.mock('@lingui/core/macro', () => ({msg: (d: unknown) => d}));
vi.mock('@app/app/I18n', () => ({default: {_: (d: {message?: string}) => d?.message ?? ''}}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {
		post: vi.fn(async (url: string, options: {body?: unknown}) => record('POST', url, options)),
		put: vi.fn(async (url: string, options: {body?: unknown}) => record('PUT', url, options)),
	},
}));
vi.mock('@app/features/messaging/utils/DesktopResourceUrl', () => ({wrapDesktopLocalUploadURL: (url: string) => url}));
vi.mock('@app/features/platform/state/AuthSession', () => ({default: {currentAccountKey: 'account'}}));
vi.mock('@app/features/platform/state/AccountScopedWork', () => ({
	AccountScopedWork: {registerCancellation: vi.fn(), currentAccountKey: 'account'},
	accountScopedWorkAbortError: () => new Error('aborted'),
}));
vi.mock('@app/features/platform/state/AccountTransitionAbort', () => ({isAccountTransitionAbortError: () => false}));
vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {features: {presigned_attachment_uploads: true}}}));
vi.mock('@app/features/user/state/PrivacyPreferences', () => ({
	default: {getPreuploadMessageAttachments: () => preupload},
}));
vi.mock('@app/features/messaging/utils/MessageAttachmentUtils', () => ({
	prepareAttachmentsForNonce: async () => prepared,
}));
vi.mock('@app/features/messaging/upload/CloudUpload', () => ({
	CloudUpload: {
		getMessageUpload: () => messageUpload,
		startSendingProgress: () => {},
		updateSendingProgress: () => {},
		updateAttachment: () => {},
		subscribeToTextarea: () => () => {},
		hasAttachment: () => true,
		removeMessageUpload: () => {},
		restoreAttachmentsToTextarea: () => {},
	},
}));
vi.mock('@app/features/messaging/commands/MessageCommands', () => ({sendError: vi.fn()}));
vi.mock('@app/features/messaging/commands/DraftCommands', () => ({createDraft: vi.fn()}));
vi.mock('@app/features/slowmode/commands/SlowmodeCommands', () => ({}));
vi.mock('@app/features/ui/commands/ModalCommands', () => ({push: vi.fn(), pushWithKey: vi.fn(), modal: vi.fn()}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({default: {}}));
vi.mock('@app/features/devtools/utils/CommandUtils', () => ({createSystemMessage: vi.fn()}));
vi.mock('@app/features/user/components/settings_utils/SettingsConstants', () => ({formatUserSettingsPath: vi.fn()}));
vi.mock('@app/features/app/components/alerts/DmActionErrorModal', () => ({showDmActionErrorModal: vi.fn()}));
vi.mock('@app/features/app/components/alerts/FeatureTemporarilyDisabledModal', () => ({}));
vi.mock('@app/features/navigation/state/SelectedChannel', () => ({default: {currentChannelId: null}}));
vi.mock('@app/features/messaging/components/alerts/AttachmentUploadConnectivityModal', () => ({}));
vi.mock('@app/features/messaging/components/alerts/FileSizeTooLargeModal', () => ({}));
vi.mock('@app/features/messaging/components/alerts/MessageSendFailedModal', () => ({}));
vi.mock('@app/features/messaging/components/alerts/MessageSendTooQuickModal', () => ({}));
vi.mock('@app/features/moderation/components/alerts/MatureContentRejectedModal', () => ({}));
vi.mock('@app/features/slowmode/components/alerts/SlowmodeRateLimitedModal', () => ({}));
vi.mock('@app/features/messaging/utils/MessageRequestUtils', () => ({buildMessageCreateRequest: vi.fn()}));
vi.mock('@app/features/threads/state/ThreadGuilds', () => ({default: {purgedThreadIds: new Set<string>()}}));

const {MessageQueue} = await import('@app/features/messaging/state/MessageQueue');

const CHANNEL = '100';

function file(name: string, size: number, type: string): File {
	return new File([new Uint8Array(size)], name, {type});
}

function singlepartPlan(id: string, filename: string, size: number) {
	return {
		upload_mode: 'singlepart',
		id,
		filename,
		upload_filename: `up/${filename}`,
		upload_url: `https://s3.test/${filename}`,
		file_size: size,
		content_type: 'image/png',
	};
}

function multipartPlan(id: string, filename: string, size: number, partSize: number) {
	const count = Math.ceil(size / partSize);
	return {
		upload_mode: 'multipart',
		id,
		filename,
		upload_filename: `up/${filename}`,
		file_size: size,
		content_type: 'video/mp4',
		upload_id: `mp-${id}`,
		part_size: partSize,
		parts: Array.from({length: count}, (_, index) => ({
			part_number: index + 1,
			upload_url: `https://s3.test/${filename}/part${index + 1}`,
		})),
	};
}

beforeEach(() => {
	requests.length = 0;
	responses.clear();
	preupload = false;
	messageUpload = null;
});

describe('MessageQueue attachment upload request sequence', () => {
	it('uploads singlepart and multipart plans at send time then completes the multipart uploads', async () => {
		const image = file('a.png', 10, 'image/png');
		const video = file('b.mp4', 20, 'video/mp4');
		prepared = {
			attachments: [
				{id: '0', filename: 'a.png'},
				{id: '1', filename: 'b.mp4'},
			],
			files: [image, video],
		};
		responses.set(`POST /channels/${CHANNEL}/attachments`, () => ({
			attachments: [singlepartPlan('0', 'a.png', 10), multipartPlan('1', 'b.mp4', 20, 10)],
		}));
		const queue = new MessageQueue();
		const result = await queue.prepareAttachmentsForSend({channelId: CHANNEL, nonce: 'n1'});
		expect(requests).toEqual([
			`POST /channels/${CHANNEL}/attachments {"attachments":[{"id":"0","filename":"a.png","file_size":10,"content_type":"image/png"},{"id":"1","filename":"b.mp4","file_size":20,"content_type":"video/mp4"}]}`,
			'PUT https://s3.test/a.png <file a.png>',
			'PUT https://s3.test/b.mp4/part1 <blob 10>',
			'PUT https://s3.test/b.mp4/part2 <blob 10>',
			`POST /channels/${CHANNEL}/attachments/complete {"uploads":[{"upload_filename":"up/b.mp4","upload_id":"mp-1"}]}`,
		]);
		expect(result).toEqual({
			attachments: [
				{id: '0', filename: 'a.png', upload_filename: 'up/a.png', file_size: 10, content_type: 'image/png'},
				{id: '1', filename: 'b.mp4', upload_filename: 'up/b.mp4', file_size: 20, content_type: 'video/mp4'},
			],
			files: undefined,
		});
	});

	it('falls back to a multipart message upload when the presign endpoint is unreachable', async () => {
		const image = file('a.png', 10, 'image/png');
		prepared = {attachments: [{id: '0', filename: 'a.png'}], files: [image]};
		responses.set(`POST /channels/${CHANNEL}/attachments`, () => {
			throw new Error('Network error during request');
		});
		const queue = new MessageQueue();
		const result = await queue.prepareAttachmentsForSend({channelId: CHANNEL, nonce: 'n2'});
		expect(requests).toHaveLength(1);
		expect(result).toEqual({attachments: prepared.attachments, files: [image]});
	});

	it('reuses textarea preuploads at send time without a second presign', async () => {
		preupload = true;
		const video = file('b.mp4', 20, 'video/mp4');
		responses.set(`POST /channels/${CHANNEL}/attachments`, () => ({
			attachments: [multipartPlan('7', 'b.mp4', 20, 20)],
		}));
		const queue = new MessageQueue();
		queue.startTextareaAttachmentUploads(CHANNEL, [
			{id: 7, file: video, filename: 'b.mp4', flags: 0} as unknown as Parameters<
				typeof queue.startTextareaAttachmentUploads
			>[1][number],
		]);
		messageUpload = {attachments: [{id: 7}]};
		prepared = {attachments: [{id: '7', filename: 'b.mp4'}], files: [video]};
		const result = await queue.prepareAttachmentsForSend({channelId: CHANNEL, nonce: 'n3'});
		expect(requests).toEqual([
			`POST /channels/${CHANNEL}/attachments {"attachments":[{"id":"7","filename":"b.mp4","file_size":20,"content_type":"video/mp4"}]}`,
			'PUT https://s3.test/b.mp4/part1 <blob 20>',
			`POST /channels/${CHANNEL}/attachments/complete {"uploads":[{"upload_filename":"up/b.mp4","upload_id":"mp-7"}]}`,
		]);
		expect(result).toEqual({
			attachments: [
				{id: '7', filename: 'b.mp4', upload_filename: 'up/b.mp4', file_size: 20, content_type: 'video/mp4'},
			],
			files: undefined,
		});
	});
});
