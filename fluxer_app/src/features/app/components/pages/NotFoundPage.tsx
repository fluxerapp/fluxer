// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import styles from '@app/features/app/components/pages/NotFoundPage.module.css';
import {Link} from '@app/features/platform/components/router/RouterReact';
import {Button} from '@app/features/ui/button/Button';
import {FluxerIcon} from '@app/features/ui/components/icons/FluxerIcon';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const PAGE_NOT_FOUND_TITLE_DESCRIPTOR = msg({
	message: 'Not found',
	comment: 'Browser tab and window title for the 404 page.',
});

export const NotFoundPage = observer(() => {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(PAGE_NOT_FOUND_TITLE_DESCRIPTOR));
	return (
		<div className={styles.container} data-flx="app.not-found-page.container">
			<FluxerIcon className={styles.icon} data-flx="app.not-found-page.icon" />
			<div className={styles.content} data-flx="app.not-found-page.content">
				<h1 className={styles.title} data-flx="app.not-found-page.title">
					<Trans>404: page not found</Trans>
				</h1>
				<p className={styles.description} data-flx="app.not-found-page.description">
					<Trans>The page you're looking for doesn't exist or has been moved.</Trans>
				</p>
			</div>
			<div className={styles.actions} data-flx="app.not-found-page.actions">
				<Link to={Routes.ME} data-flx="app.not-found-page.link">
					<Button data-flx="app.not-found-page.button">
						<Trans>Go to home</Trans>
					</Button>
				</Link>
			</div>
		</div>
	);
});
