// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForumTag} from '@app/api/models/ForumTag';
import type {ThreadParentConfig} from '@app/api/models/ThreadParentConfig';
import {UnknownForumTagError} from '@fluxer/errors/src/domains/channel/UnknownForumTagError';
import type {ForumTagRequest, ForumTagUpdateRequest} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';

export type ForumTagEdit =
	| {kind: 'create'; tag: ForumTagRequest}
	| {kind: 'update'; tagId: bigint; tag: ForumTagRequest}
	| {kind: 'delete'; tagId: bigint};

function toUpdateRequest(tag: ForumTag): ForumTagUpdateRequest {
	return {
		id: tag.id,
		name: tag.name,
		moderated: tag.moderated,
		emoji_id: tag.emojiId,
		emoji_name: tag.emojiName,
	};
}

export function applyForumTagEdit(config: ThreadParentConfig | null, edit: ForumTagEdit): Array<ForumTagUpdateRequest> {
	const tags = (config?.availableTags ?? []).map(toUpdateRequest);
	if (edit.kind === 'create') return [...tags, edit.tag];
	const index = tags.findIndex((tag) => tag.id === edit.tagId);
	if (index === -1) throw new UnknownForumTagError();
	if (edit.kind === 'delete') return tags.filter((_, position) => position !== index);
	return tags.map((tag, position) => (position === index ? {...edit.tag, id: edit.tagId} : tag));
}
