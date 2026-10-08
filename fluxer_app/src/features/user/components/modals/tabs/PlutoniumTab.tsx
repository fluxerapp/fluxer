// SPDX-License-Identifier: AGPL-3.0-or-later

import {PlutoniumContent} from '@app/features/app/components/dialogs/components/PlutoniumContent';
import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import {PlutoniumPageLinkCard} from '@app/features/premium/components/plutonium_page/PlutoniumPageLinkCard';
import PlutoniumPageRollout from '@app/features/premium/state/PlutoniumPageRollout';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const PlutoniumPageRedirect: React.FC = () => {
	useEffect(() => {
		PlutoniumPageCommands.openPlutoniumPage();
	}, []);
	return <PlutoniumPageLinkCard data-flx="user.plutonium-tab.plutonium-page-link-card" />;
};

const PlutoniumTab: React.FC = observer(() => {
	if (PlutoniumPageRollout.enabled) {
		return <PlutoniumPageRedirect data-flx="user.plutonium-tab.plutonium-page-redirect" />;
	}
	return <PlutoniumContent data-flx="user.plutonium-tab.plutonium-content" />;
});

export const PlutoniumInlineTab: React.FC = observer(() => {
	if (PlutoniumPageRollout.enabled) {
		return <PlutoniumPageLinkCard data-flx="user.plutonium-tab.inline.plutonium-page-link-card" />;
	}
	return <PlutoniumContent data-flx="user.plutonium-tab.plutonium-content" />;
});

export default PlutoniumTab;
