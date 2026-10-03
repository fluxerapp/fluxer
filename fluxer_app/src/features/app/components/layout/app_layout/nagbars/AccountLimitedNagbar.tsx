// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {ACCOUNT_LIMITED_NOTICE_DESCRIPTOR} from '@app/features/user/utils/AccountLimitUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const AccountLimitedNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.NEUTRAL].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.NEUTRAL].textColor}
			data-flx="app.app-layout.nagbars.account-limited-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={
					<span data-flx="app.app-layout.nagbars.account-limited-nagbar.message">
						{i18n._(ACCOUNT_LIMITED_NOTICE_DESCRIPTOR)}
					</span>
				}
				data-flx="app.app-layout.nagbars.account-limited-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
