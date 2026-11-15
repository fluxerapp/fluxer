// SPDX-License-Identifier: AGPL-3.0-or-later

import {getContentMessage} from '@app/api/content_i18n/ContentI18n';

export function getDonationProductData({
	interval,
	locale,
	productName,
}: {
	interval: 'month' | 'year' | null;
	locale: string | null;
	productName: string;
}) {
	const variables = {product_name: productName};
	if (interval === 'month') {
		return {
			name: getContentMessage('billing.donation_name_recurring', locale, variables),
			description: getContentMessage('billing.donation_description_monthly', locale, variables),
		};
	}
	if (interval === 'year') {
		return {
			name: getContentMessage('billing.donation_name_recurring', locale, variables),
			description: getContentMessage('billing.donation_description_yearly', locale, variables),
		};
	}
	return {
		name: getContentMessage('billing.donation_name_one_time', locale, variables),
		description: getContentMessage('billing.donation_description_one_time', locale, variables),
	};
}
