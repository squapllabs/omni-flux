// Avatar.tsx
import React from 'react';
import Styles from '../../styles/navbar.module.scss';

interface AvatarProps {
  imageUrl?: string;
  firstName: string;
  lastName: string;
  size: number;
  fontSizeChange: boolean;
}

const Avatar: React.FC<AvatarProps> = ({
  imageUrl,
  firstName,
  lastName,
  size,
  fontSizeChange = false,
}) => {
  const avatarStyle = {
    width: `${size}px`,
    height: `${size}px`,
  };
  const fontSize = size / 2;
  return (
    <div className={Styles.avatar} style={avatarStyle}>
      {imageUrl ? (
        <img src={imageUrl} alt={`${firstName} ${lastName}`} />
      ) : (
        <div
          className={Styles.initials}
          style={{ fontSize: fontSizeChange ? `${fontSize}px` : '' }}
        >
          {firstName.charAt(0)}
          {lastName.charAt(0)}
        </div>
      )}
    </div>
  );
};

export default Avatar;
