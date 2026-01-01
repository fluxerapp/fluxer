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

import type {PermissionOverwrite} from '~/database/CassandraTypes';

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
