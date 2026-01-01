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
import {type BetaCode, BetaCodeRecord} from '~/records/BetaCodeRecord';
import BetaCodeStore from '~/stores/BetaCodeStore';

const logger = new Logger('BetaCodes');

interface BetaCodesListResponse {
	beta_codes: Array<BetaCode>;
	allowance: number;
	next_reset_at: string | null;
}

export const fetch = async () => {
	BetaCodeStore.fetchPending();
	try {
		const response = await http.get<BetaCodesListResponse>({url: Endpoints.USER_BETA_CODES, retries: 1});
		const data = response.body;
		BetaCodeStore.fetchSuccess(data.beta_codes, data.allowance, data.next_reset_at);
		return data;
	} catch (error) {
		logger.error('Failed to fetch beta codes:', error);
		BetaCodeStore.fetchError();
		throw error;
	}
};

export const create = async () => {
	BetaCodeStore.createPending();
	try {
		const response = await http.post<BetaCode>(Endpoints.USER_BETA_CODES);
		const betaCode = new BetaCodeRecord(response.body);
		BetaCodeStore.createSuccess(betaCode);
		return betaCode;
	} catch (error) {
		logger.error('Failed to create beta code:', error);
		BetaCodeStore.createError();
		throw error;
	}
};

export const remove = async (code: string) => {
	if (!code) {
		throw new Error('No beta code provided');
	}
	BetaCodeStore.deletePending();
	try {
		await http.delete({url: Endpoints.USER_BETA_CODE(code)});
		BetaCodeStore.deleteSuccess(code);
	} catch (error) {
		logger.error(`Failed to delete beta code ${code}:`, error);
		BetaCodeStore.deleteError();
		throw error;
	}
};
