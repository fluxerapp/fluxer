// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import type {User} from '@app/api/models/User';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';

interface IncrementDmMentionCountsParams {
	readStateService: ReadStateService;
	userRepository: Pick<IUserRepository, 'getRelationship'>;
	user: User | null;
	recipients: Array<User>;
	channelId: ChannelID;
	messageId: MessageID;
}

export async function incrementDmMentionCounts(params: IncrementDmMentionCountsParams): Promise<void> {
	const {readStateService, userRepository, user, recipients, channelId, messageId} = params;
	if (!user) return;
	const validRecipients = recipients.filter((recipient) => recipient.id !== user.id && !recipient.isBot);
	if (validRecipients.length === 0) return;
	const mentionableRecipients = (
		await Promise.all(
			validRecipients.map(async (recipient) => {
				const block = await userRepository.getRelationship(recipient.id, user.id, RelationshipTypes.BLOCKED);
				return block ? null : recipient;
			}),
		)
	).filter((recipient): recipient is User => recipient != null);
	if (mentionableRecipients.length === 0) return;
	await readStateService.bulkIncrementMentionCounts(
		mentionableRecipients.map((recipient) => ({
			userId: recipient.id,
			channelId,
			messageId,
		})),
	);
}
