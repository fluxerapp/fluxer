// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResourceRegisterPage} from '@app/features/auth/flow/ResourceAuthPages';
import {giftAuthPageConfig} from '@app/features/expressions/components/pages/GiftAuthPageConfig';
import {useParams} from '@app/features/platform/components/router/RouterReact';

export default function GiftRegisterPage() {
	const {code} = useParams() as {code: string};
	return (
		<ResourceRegisterPage
			id={code}
			config={giftAuthPageConfig}
			data-flx="expressions.gift-register-page.resource-register-page"
		/>
	);
}
