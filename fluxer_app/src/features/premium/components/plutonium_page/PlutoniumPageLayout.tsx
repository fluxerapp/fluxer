// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {PlutoniumPage} from '@app/features/premium/components/plutonium_page/PlutoniumPage';
import styles from '@app/features/premium/components/plutonium_page/PlutoniumPageLayout.module.css';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

export const PlutoniumPageLayout = observer(function PlutoniumPageLayout() {
	const enabled = shouldShowPremiumFeatures();
	useEffect(() => {
		if (!enabled) RouterUtils.replaceWith(Routes.ME);
	}, [enabled]);
	return (
		<div className={styles.layout} data-flx="premium.plutonium-page-layout.layout">
			{enabled && <PlutoniumPage data-flx="premium.plutonium-page-layout.page" />}
		</div>
	);
});
