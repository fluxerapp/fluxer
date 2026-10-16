// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {TermsAcceptanceModal} from '@app/features/auth/components/modals/TermsAcceptanceModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const TERMS_ACCEPTANCE_MESSAGE_DESCRIPTOR = msg({
	message: "We've updated our policies. Review and accept them to continue.",
	comment: 'Nagbar body shown when the user must accept updated policies before continuing.',
});
const REVIEW_AGREE_DESCRIPTOR = msg({
	message: 'Review & agree',
	comment: 'Button label on the policy-update nagbar. Opens the terms acceptance modal.',
});
export const TermsAcceptanceNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const handleOpen = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<TermsAcceptanceModal data-flx="app.app-layout.nagbars.terms-acceptance-nagbar.handle-open.terms-acceptance-modal" />
			)),
		);
	}, []);
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.LEGAL].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.LEGAL].textColor}
			data-flx="app.app-layout.nagbars.terms-acceptance-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(TERMS_ACCEPTANCE_MESSAGE_DESCRIPTOR)}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpen}
						data-flx="app.app-layout.nagbars.terms-acceptance-nagbar.nagbar-button.open"
					>
						{i18n._(REVIEW_AGREE_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.terms-acceptance-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
