// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResourceRegisterPage} from '@app/features/auth/flow/ResourceAuthPages';
import {useParams} from '@app/features/platform/components/router/RouterReact';
import {themeAuthPageConfig} from '@app/features/theme/components/pages/ThemeAuthPageConfig';

export default function ThemeRegisterPage() {
	const {themeId} = useParams() as {themeId: string};
	return (
		<ResourceRegisterPage
			id={themeId}
			config={themeAuthPageConfig}
			data-flx="theme.theme-register-page.resource-register-page"
		/>
	);
}
