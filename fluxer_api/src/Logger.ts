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

import pino from 'pino';
import {Config} from '~/Config';

export const Logger = pino({
	level: Config.nodeEnv === 'development' ? 'debug' : 'info',
	transport:
		Config.nodeEnv === 'development'
			? {
					target: 'pino-pretty',
					options: {
						colorize: true,
						translateTime: 'HH:MM:ss.l',
						ignore: 'pid,hostname',
						messageFormat: '{msg}',
					},
				}
			: undefined,
	formatters: {
		level: (label) => ({level: label}),
	},
	errorKey: 'error',
	serializers: {
		reason: (value) => {
			if (value instanceof Error) {
				return pino.stdSerializers.err(value);
			}
			return value;
		},
	},
	timestamp: pino.stdTimeFunctions.isoTime,
	base: {
		service: 'fluxer-api',
	},
});
