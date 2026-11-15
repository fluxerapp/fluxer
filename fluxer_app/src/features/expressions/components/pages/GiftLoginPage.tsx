// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResourceLoginPage} from '@app/features/auth/flow/ResourceAuthPages';
import {giftAuthPageConfig} from '@app/features/expressions/components/pages/GiftAuthPageConfig';
import {useParams} from '@app/features/platform/components/router/RouterReact';

export default function GiftLoginPage() {
	const {code} = useParams() as {code: string};
	return (
		<ResourceLoginPage
			id={code}
			config={giftAuthPageConfig}
			data-flx="expressions.gift-login-page.resource-login-page"
		/>
	);
}
