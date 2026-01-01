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

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {BackupCode} from '~/records/UserRecord';
import SudoStore from '~/stores/SudoStore';

const logger = new Logger('MFA');

export const enableMfaTotp = async (secret: string, code: string): Promise<Array<BackupCode>> => {
	try {
		logger.debug('Enabling TOTP-based MFA');
		const response = await http.post<{backup_codes: Array<BackupCode>}>({
			url: Endpoints.USER_MFA_TOTP_ENABLE,
			body: {secret, code},
		});
		const result = response.body;
		logger.debug('Successfully enabled TOTP-based MFA');
		SudoStore.clearToken();
		return result.backup_codes;
	} catch (error) {
		logger.error('Failed to enable TOTP-based MFA:', error);
		throw error;
	}
};

export const disableMfaTotp = async (code: string): Promise<void> => {
	try {
		logger.debug('Disabling TOTP-based MFA');
		await http.post({url: Endpoints.USER_MFA_TOTP_DISABLE, body: {code}});
		logger.debug('Successfully disabled TOTP-based MFA');
	} catch (error) {
		logger.error('Failed to disable TOTP-based MFA:', error);
		throw error;
	}
};

export const getBackupCodes = async (regenerate = false): Promise<Array<BackupCode>> => {
	try {
		logger.debug(`${regenerate ? 'Regenerating' : 'Fetching'} MFA backup codes`);
		const response = await http.post<{backup_codes: Array<BackupCode>}>({
			url: Endpoints.USER_MFA_BACKUP_CODES,
			body: {regenerate},
		});
		const result = response.body;

		logger.debug(`Successfully ${regenerate ? 'regenerated' : 'fetched'} MFA backup codes`);
		return result.backup_codes;
	} catch (error) {
		logger.error(`Failed to ${regenerate ? 'regenerate' : 'fetch'} MFA backup codes:`, error);
		throw error;
	}
};
