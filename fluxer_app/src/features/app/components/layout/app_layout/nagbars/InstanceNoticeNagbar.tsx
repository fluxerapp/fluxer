// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	instanceNoticeDismissals,
	useVisibleInstanceNotice,
} from '@app/features/app/components/layout/app_layout/nagbars/InstanceNoticeState';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const LEARN_MORE_DESCRIPTOR = msg({
	message: 'Learn more',
	comment:
		'Button on the banner with a notice from the instance administrator. Opens the link the administrator added.',
});

export const InstanceNoticeNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const notice = useVisibleInstanceNotice();
	if (!notice) {
		return null;
	}
	const tone = NAGBAR_TONES[NagbarToneKind.MAINTENANCE];
	const handleDismiss = () => instanceNoticeDismissals.dismiss(notice.id);
	const url = notice.url;
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={tone.backgroundColor}
			textColor={tone.textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.instance-notice-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={notice.message}
				actions={
					url ? (
						<NagbarButton
							isMobile={isMobile}
							onClick={() => openExternalUrlWithWarning(url)}
							data-flx="app.app-layout.nagbars.instance-notice-nagbar.nagbar-button.open-link"
						>
							{i18n._(LEARN_MORE_DESCRIPTOR)}
						</NagbarButton>
					) : undefined
				}
				data-flx="app.app-layout.nagbars.instance-notice-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
