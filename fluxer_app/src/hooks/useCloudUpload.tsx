/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import {type CloudAttachment, CloudUpload, type MessageUpload} from '~/lib/CloudUpload';

export function useTextareaAttachments(channelId: string): ReadonlyArray<CloudAttachment> {
	const [attachments, setAttachments] = React.useState<ReadonlyArray<CloudAttachment>>(() =>
		CloudUpload.getTextareaAttachments(channelId),
	);

	React.useEffect(() => {
		const subscription = CloudUpload.attachments$(channelId).subscribe(setAttachments);
		return () => subscription.unsubscribe();
	}, [channelId]);

	return attachments;
}

export function useMessageUpload(nonce: string): MessageUpload | null {
	const [upload, setUpload] = React.useState<MessageUpload | null>(() =>
		nonce ? CloudUpload.getMessageUpload(nonce) : null,
	);

	React.useEffect(() => {
		if (!nonce) {
			setUpload(null);
			return;
		}

		const subscription = CloudUpload.messageUpload$(nonce).subscribe((next) => {
			setUpload(next);
		});

		return () => subscription.unsubscribe();
	}, [nonce]);

	return upload;
}
