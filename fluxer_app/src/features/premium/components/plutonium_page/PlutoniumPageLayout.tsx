// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {PlutoniumPage} from '@app/features/premium/components/plutonium_page/PlutoniumPage';
import styles from '@app/features/premium/components/plutonium_page/PlutoniumPageLayout.module.css';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {getPremiumPageVariables} from '@app/features/theme/utils/InstanceBrandColors';
import {observer} from 'mobx-react-lite';
import {type CSSProperties, useEffect} from 'react';

export const PlutoniumPageLayout = observer(function PlutoniumPageLayout() {
	const enabled = shouldShowPremiumFeatures();
	const pageColors = getPremiumPageVariables(
		RuntimeConfig.isSelfHosted() ? RuntimeConfig.appPublic.branding.premium_page_color : null,
	);
	useEffect(() => {
		if (!enabled) RouterUtils.replaceWith(Routes.ME);
	}, [enabled]);
	return (
		<div
			className={styles.layout}
			style={pageColors ? ({'--pl-layout-face': pageColors['--pl-layout-face']} as CSSProperties) : undefined}
			data-flx="premium.plutonium-page-layout.layout"
		>
			{enabled && <PlutoniumPage data-flx="premium.plutonium-page-layout.page" />}
		</div>
	);
});
