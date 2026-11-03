// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {scales, tails} from '@app/api/utils/UsernameGenerator';

export function generateConnectionId(): string {
	return `${tails[randomInt(tails.length)]}-${scales[randomInt(scales.length)]}`;
}
