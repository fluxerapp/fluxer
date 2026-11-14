// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResourceLoginPage} from '@app/features/auth/flow/ResourceAuthPages';
import {useParams} from '@app/features/platform/components/router/RouterReact';
import {themeAuthPageConfig} from '@app/features/theme/components/pages/ThemeAuthPageConfig';

export default function ThemeLoginPage() {
	const {themeId} = useParams() as {themeId: string};
	return (
		<ResourceLoginPage
			id={themeId}
			config={themeAuthPageConfig}
			data-flx="theme.theme-login-page.resource-login-page"
		/>
	);
}
