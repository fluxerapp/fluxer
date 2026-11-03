// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AUTHORIZE_APPLICATION_DESCRIPTOR,
	OAuthAuthorizeFlowPanel,
} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizeFlowPanel';
import {useAuthorizeFlow} from '@app/features/auth/components/pages/oauth_authorize_page/state/useAuthorizeFlow';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

interface OAuthAuthorizeSessionProps {
	readonly startAtConsent: boolean;
	readonly onAccountChanged: () => void;
}

const OAuthAuthorizeSession = observer(function OAuthAuthorizeSession({
	startAtConsent,
	onAccountChanged,
}: OAuthAuthorizeSessionProps): React.ReactElement {
	const flow = useAuthorizeFlow({startAtConsent});
	const choosingAccount =
		flow.phase.kind === 'session_expired' || (flow.phase.kind === 'review' && flow.phase.step === 'account');
	useAuthPresentation({variant: choosingAccount ? 'standard' : 'compact'});
	return (
		<OAuthAuthorizeFlowPanel
			flow={flow}
			onAccountChanged={onAccountChanged}
			data-flx="auth.o-auth-authorize-page.o-auth-authorize-flow-panel"
		/>
	);
});

const OAuthAuthorizePage: React.FC = observer(() => {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(AUTHORIZE_APPLICATION_DESCRIPTOR));
	const [accountGeneration, setAccountGeneration] = useState(0);
	const handleAccountChanged = useCallback(() => {
		setAccountGeneration((generation) => generation + 1);
	}, []);
	return (
		<OAuthAuthorizeSession
			key={accountGeneration}
			startAtConsent={accountGeneration > 0}
			onAccountChanged={handleAccountChanged}
			data-flx="auth.o-auth-authorize-page.o-auth-authorize-session"
		/>
	);
});

export default OAuthAuthorizePage;
