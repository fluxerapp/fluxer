// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/auth/flow/AuthPageStyles.module.css';
import type {LoadableErrorProps} from '@app/features/platform/components/loadable/LoadableComponent';
import {Button} from '@app/features/ui/button/Button';
import {ph} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import type React from 'react';

export function AuthRouteLoadError({retry}: LoadableErrorProps): React.JSX.Element {
	return (
		<div className={styles.errorContainer} data-flx="auth.flow.auth-route-load-error.container">
			<div className={styles.errorTitle} data-flx="auth.flow.auth-route-load-error.title">
				<Trans>Could not load this page</Trans>
			</div>
			<div className={styles.errorText} data-flx="auth.flow.auth-route-load-error.text">
				<Trans>Try again, or refresh {ph({productName: RuntimeConfig.productName})} if the problem continues.</Trans>
			</div>
			<div className={styles.disabledActions} data-flx="auth.flow.auth-route-load-error.actions">
				<Button onClick={retry} fitContainer data-flx="auth.flow.auth-route-load-error.retry">
					<Trans>Try again</Trans>
				</Button>
			</div>
		</div>
	);
}
