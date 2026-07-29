import type React from 'react';
import styles from './NativeTitlebarNav.module.css';
import { canGoBack, goBackOr, onCanGoBackChange} from '@app/features/platform/components/router/NavigationAdapter';
import { useEffect, useState } from 'react';
import { type NativeTitlebarNavOptions } from '@app/features/ui/utils/NativeUtils';

interface NativeTitlebarNavProps {
    direction: NativeTitlebarNavOptions;
}
//import { u}

export const NativeTitlebarNav: React.FC<NativeTitlebarNavProps> = ( {direction} ) => {
    const [backAvailable, setBackAvailable] = useState(canGoBack);

    useEffect(() => {
        return onCanGoBackChange(() => setBackAvailable(canGoBack()));
    }, []);

    const handleBack = () => goBackOr('/');
    const handleForward = () => window.history.forward();

    return (
        <div 
         className={styles.nav} data-flx="app.native-titlebar-nav">
            {/* back & forward arrow btns & icons */}
            { direction === "back" ? (
            <button
                type="button"
                disabled={backAvailable}
                className={styles.button}
                onClick={handleBack}
                aria-label="Go back"
                data-flx="app.native-titlebar-nav.back"
            >
                ←
            </button>
            ) : (
            <button
                type="button"
                className={styles.button}
                onClick={handleForward}
                aria-label="Go forward"
                data-flx="app.native-titlebar-nav.forward"
            >
                →
            </button>
            )}
        </div>
    );
}