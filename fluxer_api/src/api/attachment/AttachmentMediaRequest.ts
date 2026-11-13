// SPDX-License-Identifier: AGPL-3.0-or-later

import {isApiProxiedAttachments} from '@app/api/instance/AttachmentAccessMode';
import {stripApiPrefix} from '@app/api/utils/RequestPathUtils';

const ATTACHMENT_MEDIA_PATH_REGEX = /^\/channels\/[0-9]{1,20}\/attachments\/[0-9]{1,20}\/.+$/u;

export function isAttachmentMediaRequest(method: string, path: string): boolean {
	return (
		isApiProxiedAttachments() &&
		(method === 'GET' || method === 'HEAD') &&
		ATTACHMENT_MEDIA_PATH_REGEX.test(stripApiPrefix(path))
	);
}
