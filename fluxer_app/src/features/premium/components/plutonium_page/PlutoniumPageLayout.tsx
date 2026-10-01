// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {PlutoniumPage} from '@app/features/premium/components/plutonium_page/PlutoniumPage';
import styles from '@app/features/premium/components/plutonium_page/PlutoniumPageLayout.module.css';
import PlutoniumPageRollout from '@app/features/premium/state/PlutoniumPageRollout';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

export const PlutoniumPageLayout = observer(function PlutoniumPageLayout() {
	const ready = PlutoniumPageRollout.assignmentReady;
	const enabled = PlutoniumPageRollout.enabled && shouldShowPremiumFeatures();
	useEffect(() => {
		if (!ready || enabled) return;
		RouterUtils.replaceWith(Routes.ME);
		if (!shouldShowPremiumFeatures()) return;
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="plutonium"
					data-flx="premium.plutonium-page-layout.fallback.user-settings-modal"
				/>
			)),
		);
	}, [enabled, ready]);
	return (
		<div className={styles.layout} data-flx="premium.plutonium-page-layout.layout">
			{enabled && <PlutoniumPage data-flx="premium.plutonium-page-layout.page" />}
		</div>
	);
});

export default PlutoniumPageLayout;
