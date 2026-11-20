// SPDX-License-Identifier: AGPL-3.0-or-later

import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import {UploadingAttachment} from '@app/features/messaging/models/UploadingAttachment';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Personas from '@app/features/user/state/Personas';
import Users from '@app/features/user/state/Users';
import {MessageFlags, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

const logger = new Logger('VoiceMessageSendUtils');

export interface SendVoiceMessageParams {
	channelId: string;
	file: File;
	waveform: string;
	duration: number;
	title?: string;
}

export async function sendVoiceMessage(params: SendVoiceMessageParams): Promise<void> {
	const {channelId, file, waveform, duration, title} = params;
	const [uploaded] = await CloudUpload.createAndStartUploads(channelId, [file]);
	uploaded.waveform = waveform;
	uploaded.duration = duration;
	uploaded.isVoiceMessage = true;
	const nonce = SnowflakeUtils.fromTimestamp(Date.now());
	CloudUpload.claimAttachmentsForMessage(channelId, nonce, [uploaded], {
		content: '',
		flags: MessageFlags.VOICE_MESSAGE,
	});
	const currentUser = Users.getCurrentUser();
	if (!currentUser) {
		throw new Error('Current user missing');
	}
	const persona = Personas.getGlobalActivePersona();
	const uploadingAttachment = UploadingAttachment.fromDescriptor({
		filename: file.name,
		title: title ?? file.name,
		size: file.size,
		contentType: file.type,
	}).toJSON();
	const message = new Message({
		id: nonce,
		channel_id: channelId,
		author: currentUser.toJSON(),
		type: MessageTypes.DEFAULT,
		flags: MessageFlags.VOICE_MESSAGE,
		pinned: false,
		mention_everyone: false,
		content: '',
		timestamp: new Date().toISOString(),
		mentions: [],
		state: MessageStates.SENDING,
		nonce,
		attachments: [uploadingAttachment],
		persona: persona?.toSnapshot(),
	});
	MessageCommands.createOptimistic(channelId, {...message.toJSON(), attachments: [uploadingAttachment]});
	try {
		await MessageCommands.send(channelId, {
			content: '',
			nonce,
			hasAttachments: true,
			flags: MessageFlags.VOICE_MESSAGE,
			persona: persona?.toSnapshot(),
		});
	} catch (error) {
		logger.error({error}, 'Failed to dispatch voice message');
		throw error;
	}
}
