// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MuteConfig} from '@app/api/database/types/UserTypes';

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
