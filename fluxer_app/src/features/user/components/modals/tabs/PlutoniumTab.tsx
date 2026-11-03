// SPDX-License-Identifier: AGPL-3.0-or-later

import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import {PlutoniumPageLinkCard} from '@app/features/premium/components/plutonium_page/PlutoniumPageLinkCard';
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
	return <PlutoniumPageRedirect data-flx="user.plutonium-tab.plutonium-page-redirect" />;
});

export const PlutoniumInlineTab: React.FC = observer(() => {
	return <PlutoniumPageLinkCard data-flx="user.plutonium-tab.inline.plutonium-page-link-card" />;
});

export default PlutoniumTab;
