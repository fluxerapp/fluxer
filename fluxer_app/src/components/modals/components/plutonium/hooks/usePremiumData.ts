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

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import type {PriceIds, VisionarySlots} from '~/actions/PremiumActionCreators';
import * as PremiumActionCreators from '~/actions/PremiumActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Logger} from '~/lib/Logger';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';

const logger = new Logger('usePremiumData');

export interface PremiumData {
	visionarySlots: VisionarySlots | null;
	priceIds: PriceIds | null;
	loadingSlots: boolean;
	slotsError: boolean;
	isVisionarySoldOut: boolean;
}

export const usePremiumData = (countryCode: string | null): PremiumData => {
	const {t} = useLingui();
	const [visionarySlots, setVisionarySlots] = React.useState<VisionarySlots | null>(null);
	const [priceIds, setPriceIds] = React.useState<PriceIds | null>(null);
	const [loadingSlots, setLoadingSlots] = React.useState(true);
	const [slotsError, setSlotsError] = React.useState(false);

	React.useEffect(() => {
		let mounted = true;
		const fetchData = async () => {
			try {
				const [slots, prices] = await Promise.all([
					PremiumActionCreators.fetchVisionarySlots(),
					PremiumActionCreators.fetchPriceIds(countryCode ?? undefined),
				]);
				if (!mounted) return;
				setVisionarySlots(slots);
				setPriceIds(prices);
				setLoadingSlots(false);
			} catch (error) {
				logger.error('Failed to fetch premium data', error);
				if (!mounted) return;
				ToastActionCreators.error(t`Failed to load premium information. Please try again later.`);
				setSlotsError(true);
				setLoadingSlots(false);
			}
		};
		fetchData();
		return () => {
			mounted = false;
		};
	}, [countryCode]);

	const mockSoldOut = DeveloperOptionsStore.mockVisionarySoldOut;
	const mockRemaining = DeveloperOptionsStore.mockVisionaryRemaining;
	let derivedSlots: VisionarySlots | null = visionarySlots;
	if (mockSoldOut) {
		derivedSlots = derivedSlots ? {...derivedSlots, remaining: 0} : {total: 1000, remaining: 0};
	} else if (mockRemaining !== null && mockRemaining !== undefined) {
		const total = derivedSlots?.total ?? Math.max(mockRemaining, 1);
		derivedSlots = {total, remaining: Math.max(0, mockRemaining)};
	}

	const isVisionarySoldOut = !loadingSlots && derivedSlots?.remaining === 0;

	return {
		visionarySlots: derivedSlots,
		priceIds,
		loadingSlots,
		slotsError,
		isVisionarySoldOut,
	};
};
