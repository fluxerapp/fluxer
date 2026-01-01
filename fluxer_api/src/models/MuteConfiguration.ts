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

import type {MuteConfig} from '~/database/CassandraTypes';

export class MuteConfiguration {
	readonly endTime: Date | null;
	readonly selectedTimeWindow: number | null;

	constructor(config: MuteConfig) {
		this.endTime = config.end_time ?? null;
		this.selectedTimeWindow = config.selected_time_window ?? null;
	}

	toMuteConfig(): MuteConfig {
		return {
			end_time: this.endTime,
			selected_time_window: this.selectedTimeWindow,
		};
	}
}
