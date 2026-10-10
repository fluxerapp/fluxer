// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {AttachmentDecayService} from '@app/api/attachment/AttachmentDecayService';
import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import {attachmentDecayPayloadsForMessage} from '@app/api/channel/services/message/MessageHelpers';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const CHANNEL_PAGE_SIZE = 1000;
const MESSAGE_PAGE_SIZE = 500;

async function reconcileChannel(service: AttachmentDecayService, channelId: ChannelID): Promise<number> {
	const {channelRepository} = getWorkerDependencies();
	let cursor: MessageID | undefined;
	let changed = 0;
	while (true) {
		const messages = await channelRepository.messages.listMessages(channelId, cursor, MESSAGE_PAGE_SIZE);
		if (messages.length === 0) break;
		changed += await service.reconcile(messages.flatMap(attachmentDecayPayloadsForMessage));
		cursor = messages[messages.length - 1]!.id;
		if (messages.length < MESSAGE_PAGE_SIZE) break;
	}
	return changed;
}

export async function reconcileAttachmentDecayRecords(): Promise<void> {
	const {instanceConfigRepository} = getWorkerDependencies();
	const repo = new AttachmentDecayRepository();
	const service = new AttachmentDecayService(repo, () => instanceConfigRepository.getEffectiveAttachmentDecayConfig());
	let pageState: string | null = null;
	let previousChannelId: ChannelID | null = null;
	let channels = 0;
	let changed = 0;
	do {
		const page = await repo.scanChannelsWithMessagesPage(CHANNEL_PAGE_SIZE, pageState);
		pageState = page.pageState;
		for (const channelId of page.channelIds) {
			if (channelId === previousChannelId) continue;
			previousChannelId = channelId;
			if (!(await instanceConfigRepository.getEffectiveAttachmentDecayConfig()).enabled) {
				Logger.info({channels, changed}, 'Attachment decay disabled; stopping reconcileAttachmentDecay task');
				return;
			}
			changed += await reconcileChannel(service, channelId);
			channels++;
		}
	} while (pageState);
	Logger.info({channels, changed}, 'Reconciled attachment decay records with the instance settings');
}

const reconcileAttachmentDecay: WorkerTaskHandler = async () => {
	await reconcileAttachmentDecayRecords();
};

export default reconcileAttachmentDecay;
