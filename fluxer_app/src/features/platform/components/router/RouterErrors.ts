// SPDX-License-Identifier: AGPL-3.0-or-later

import {NotFound} from '@app/features/platform/components/router/RouterTypes';

export function notFound(message?: string): NotFound {
	return new NotFound(message);
}
