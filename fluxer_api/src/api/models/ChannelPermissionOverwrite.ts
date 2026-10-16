// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PermissionOverwrite} from '@app/api/database/types/ChannelTypes';

export class ChannelPermissionOverwrite {
	readonly type: number;
	readonly allow: bigint;
	readonly deny: bigint;

	constructor(overwrite: PermissionOverwrite) {
		this.type = overwrite.type;
		this.allow = overwrite.allow_ ?? 0n;
		this.deny = overwrite.deny_ ?? 0n;
	}

	toPermissionOverwrite(): PermissionOverwrite {
		return {
			type: this.type,
			allow_: this.allow,
			deny_: this.deny,
		};
	}
}
