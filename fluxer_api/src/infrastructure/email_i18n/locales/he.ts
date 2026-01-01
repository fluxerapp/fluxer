/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmailTranslations} from '../types';

export const he: EmailTranslations = {
	passwordReset: {
		subject: 'איפוס סיסמה לחשבון Fluxer שלך',
		body: `שלום {username},

ביקשת לאפס את הסיסמה לחשבון ה-Fluxer שלך. אנא עקוב אחר הקישור שלמטה כדי להגדיר סיסמה חדשה:

{resetUrl}

אם לא ביקשת איפוס סיסמה, ניתן להתעלם מהודעה זו בבטחה.

תוקף הקישור יפוג בעוד שעה.

- צוות Fluxer`,
	},
	emailVerification: {
		subject: 'אימות כתובת האימייל שלך ב-Fluxer',
		body: `שלום {username},

אנא אמת את כתובת האימייל של חשבון ה-Fluxer שלך על ידי לחיצה על הקישור:

{verifyUrl}

אם לא יצרת חשבון Fluxer, ניתן להתעלם מהודעה זו בבטחה.

תוקף הקישור יפוג בעוד 24 שעות.

- צוות Fluxer`,
	},
	ipAuthorization: {
		subject: 'אישור התחברות מכתובת IP חדשה',
		body: `שלום {username},

זיהינו ניסיון התחברות לחשבון ה-Fluxer שלך מכתובת IP חדשה:

כתובת IP: {ipAddress}
מיקום: {location}

אם זה היית אתה, אנא אשר את כתובת ה-IP באמצעות הקישור:

{authUrl}

אם לא ניסית להתחבר, יש לשנות את הסיסמה מיד.

קישור האישור יפוג בעוד 30 דקות.

- צוות Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'החשבון שלך ב-Fluxer הושבת זמנית',
		body: `שלום {username},

החשבון שלך ב-Fluxer הושבת זמנית בעקבות פעילות חשודה.

{reason, select,
	null {}
	other {סיבה: {reason}

}}על מנת לשחזר את הגישה לחשבונך, עליך לאפס את הסיסמה:

{forgotUrl}

לאחר איפוס הסיסמה תוכל להתחבר מחדש.

אם אתה מאמין שהפעולה בוצעה בטעות, אנא פנה לצוות התמיכה שלנו.

- צוות האבטחה של Fluxer`,
	},
	accountTempBanned: {
		subject: 'החשבון שלך ב-Fluxer הושעה זמנית',
		body: `שלום {username},

החשבון שלך ב-Fluxer הושעה זמנית עקב הפרת תנאי השירות או כללי הקהילה.

משך השעיה: {durationHours, plural,
	=1 {שעה אחת}
	other {# שעות}
}
מושעה עד: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
סיבה: {reason}}
}

במהלך תקופה זו לא תהיה לך גישה לחשבון.

אנו ממליצים לעיין ב:
- תנאי השירות: {termsUrl}
- כללי הקהילה: {guidelinesUrl}

אם אתה מאמין שההחלטה שגויה או בלתי מוצדקת, תוכל להגיש ערעור ל-appeals@fluxer.app מהאימייל הזה. אנא פרט מדוע אתה מאמין שההחלטה אינה נכונה. אנו נבחן את הערעור ונשיב עם החלטתנו.

- צוות האבטחה של Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'החשבון שלך ב-Fluxer מתוכנן למחיקה',
		body: `שלום {username},

החשבון שלך ב-Fluxer מתוכנן למחיקה קבועה עקב הפרת תנאי השירות או כללי הקהילה.

תאריך מחיקה מתוכנן: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
סיבה: {reason}}
}

מדובר בפעולת אכיפה משמעותית. נתוני החשבון יימחקו לצמיתות בתאריך המתוכנן.

אנו ממליצים לעיין ב:
- תנאי השירות: {termsUrl}
- כללי הקהילה: {guidelinesUrl}

תהליך הערעור:
אם אתה מאמין שההחלטה שגויה או בלתי מוצדקת, עומדים לרשותך 30 ימים לשלוח ערעור ל-appeals@fluxer.app מהאימייל הזה.

בערעור שלך:
- פרט בבירור מדוע ההחלטה שגויה או בלתי מוצדקת
- צרף הוכחות או מידע רלוונטי

צוות האבטחה של Fluxer יבחן את הערעור וייתכן שיעכב את המחיקה עד לקבלת החלטה סופית.

- צוות האבטחה של Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'מחיקת חשבון ה-Fluxer שלך נקבעה',
		body: `שלום {username},

עצוב לנו לראות אותך עוזב! מחיקת חשבון ה-Fluxer שלך נקבעה.

תאריך מחיקה מתוכנן: {deletionDate, date, full} {deletionDate, time, short}

חשוב: ניתן לבטל את המחיקה בכל עת לפני {deletionDate, date, full} {deletionDate, time, short} על ידי התחברות מחדש לחשבון.

לפני שאתה עוזב:
לוח הפרטיות בהגדרות המשתמש מאפשר לך:
- למחוק את הודעותיך בפלטפורמה
- לייצא נתונים חשובים לפני עזיבה

לתשומת ליבך: לאחר מחיקת החשבון, לא ניתן יהיה למחוק את ההודעות. אם ברצונך למחוק הודעות, עשה זאת מראש מלוח הפרטיות.

אם שינית את דעתך – פשוט התחבר מחדש כדי לבטל את המחיקה.

- צוות Fluxer`,
	},
	inactivityWarning: {
		subject: 'החשבון שלך ב-Fluxer יימחק עקב חוסר פעילות',
		body: `שלום {username},

שמנו לב שלא התחברת לחשבון ה-Fluxer שלך במשך יותר משנתיים.

התחברות אחרונה: {lastActiveDate, date, full} {lastActiveDate, time, short}

בהתאם למדיניות שמירת הנתונים שלנו, חשבונות לא פעילים מתוזמנים למחיקה אוטומטית. חשבונך יימחק לצמיתות ב:

תאריך מחיקה מתוכנן: {deletionDate, date, full} {deletionDate, time, short}

כיצד לשמור על החשבון:
פשוט התחבר ל-{loginUrl} לפני תאריך המחיקה כדי לבטל את המחיקה האוטומטית. אין צורך בפעולה נוספת.

אם לא תתחבר:
- החשבון וכל נתוניו יימחקו לצמיתות
- ההודעות שלך יאונונימיות (ישויכו ל"משתמש שנמחק")
- הפעולה אינה הפיכה

רוצה למחוק את הודעותיך?
אם אתה מעוניין למחוק הודעות לפני מחיקת החשבון, פשוט התחבר והשתמש בלוח הפרטיות.

מקווים לראותך שוב ב-Fluxer!

- צוות Fluxer`,
	},
	harvestCompleted: {
		subject: 'ייצוא הנתונים שלך מ-Fluxer מוכן',
		body: `שלום {username},

ייצוא הנתונים שלך הושלם והוא מוכן להורדה!

סיכום הייצוא:
- סך כל ההודעות: {totalMessages, number}
- גודל הקובץ: {fileSizeMB} מ״ב
- פורמט: ארכיון ZIP עם קבצי JSON

הורד את הנתונים שלך: {downloadUrl}

חשוב: קישור זה יפוג בתאריך {expiresAt, date, full} {expiresAt, time, short}

הייצוא כולל:
- את כל הודעותיך, מאורגנות לפי ערוץ
- מטא־נתונים של הערוצים
- פרופיל משתמש ומידע על החשבון
- חברות בגילדות והגדרות
- סשנים של אימות ומידע אבטחתי

הנתונים מסודרים בפורמט JSON למען ניתוח קל.

לשאלות נוספות: support@fluxer.app

- צוות Fluxer`,
	},
	unbanNotification: {
		subject: 'השעיית חשבון ה-Fluxer שלך הוסרה',
		body: `שלום {username},

חדשות טובות! השעיית חשבון ה-Fluxer שלך הוסרה.

סיבה: {reason}

כעת תוכל להתחבר שוב ולהמשיך להשתמש ב-Fluxer.

- צוות האבטחה של Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'החשבון שלך ב-Fluxer מתוכנן למחיקה',
		body: `שלום {username},

החשבון שלך ב-Fluxer נקבע למחיקה קבועה.

תאריך מחיקה מתוכנן: {deletionDate, date, full} {deletionDate, time, short}
סיבה: {reason}

מדובר בפעולת אכיפה משמעותית. כל נתוני החשבון יימחקו לצמיתות בתאריך זה.

אם אתה מאמין שההחלטה שגויה, תוכל לשלוח ערעור ל-appeals@fluxer.app.

- צוות האבטחה של Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'הטבת Fluxer Premium שלך בוטלה',
		body: `שלום {username},

אנו מודיעים כי הטבת ה-Fluxer Premium שקיבלת בוטלה עקב מחלוקת תשלום (chargeback) שהוגשה על ידי הרוכש המקורי.

ההטבות הוסרו מהחשבון שלך. פעולה זו בוצעה מכיוון שהתשלום בוטל/הוחזר.

לשאלות: support@fluxer.app

- צוות Fluxer`,
	},
	reportResolved: {
		subject: 'הדיווח שלך ל-Fluxer נבדק',
		body: `שלום {username},

הדיווח שלך (מזהה: {reportId}) נבדק על ידי צוות האבטחה שלנו.

תגובת צוות האבטחה:
{publicComment}

תודה על תרומתך לשמירה על בטיחות הקהילה. אנו מתייחסים ברצינות לכל דיווח ומעריכים את מעורבותך.

אם יש לך שאלות או חששות, פנה אלינו בכתובת safety@fluxer.app.

- צוות האבטחה של Fluxer`,
	},
	dsaReportVerification: {
		subject: 'אמת את כתובת האימייל שלך לדיווח DSA',
		body: `שלום,

השתמש בקוד האימות הבא כדי להגיש את דיווח חוק השירותים הדיגיטליים שלך ב-Fluxer:

{code}

קוד זה יפוג בתאריך {expiresAt, date, full} {expiresAt, time, short}.

אם לא ביקשת זאת, אנא התעלם מהודעה זו.

- צוות האבטחה של Fluxer`,
	},
	registrationApproved: {
		subject: 'הרישום שלך ל-Fluxer אושר',
		body: `שלום {username},

חדשות מעולות! הרישום שלך ל-Fluxer אושר.

אתה יכול להתחבר לאפליקציה בכתובת:
{channelsUrl}

ברוך הבא לקהילת Fluxer!

- צוות Fluxer`,
	},
	emailChangeRevert: {
		subject: 'כתובת האימייל שלך ב-Fluxer השתנתה',
		body: `היי {username},

כתובת האימייל של חשבון Fluxer שלך שונתה ל-{newEmail}.

אם את/ה ביצעת את השינוי, אין צורך לעשות דבר. אם לא, אפשר לבטל ולהגן על החשבון דרך הקישור הזה:

{revertUrl}

זה ישחזר את האימייל הקודם, ינתק אותך מכל ההתקנים, יסיר מספרי טלפון מקושרים, יבטל MFA וידרוש סיסמה חדשה.

- צוות האבטחה של Fluxer`,
	},
};
