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

import {useCallback, useEffect, useState} from 'react';
import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';

const logger = new Logger('PushSubscriptions');

export interface PushSubscriptionInfo {
	subscription_id: string;
	user_agent: string | null;
}

export const usePushSubscriptions = (enabled: boolean) => {
	const [subscriptions, setSubscriptions] = useState<Array<PushSubscriptionInfo>>([]);
	const [loading, setLoading] = useState(false);

	const fetchSubscriptions = useCallback(async () => {
		if (!enabled) {
			setSubscriptions([]);
			return;
		}

		setLoading(true);
		try {
			const response = await http.get<{subscriptions: Array<PushSubscriptionInfo>}>({
				url: Endpoints.USER_PUSH_SUBSCRIPTIONS,
			});
			setSubscriptions(response.body.subscriptions ?? []);
		} catch (error) {
			logger.error('Failed to load push subscriptions', {error});
			setSubscriptions([]);
		} finally {
			setLoading(false);
		}
	}, [enabled]);

	useEffect(() => {
		void fetchSubscriptions();
	}, [fetchSubscriptions]);

	return {
		subscriptions,
		loading,
		refresh: fetchSubscriptions,
	};
};
