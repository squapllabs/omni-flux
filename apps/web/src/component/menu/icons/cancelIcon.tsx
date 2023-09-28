import React, { SVGProps } from 'react';

interface CancelIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: number;
  width?: number;
}

const RejectIcon: React.FC<CancelIconProps> = ({
  color = 'black',
  height = 40,
  width = 40,
  ...props
}) => (
  <div title="Reject">
    <svg
      xmlns="http://www.w3.org/2000/svg"
      height={height}
      width={width}
      viewBox="0 0 40 22"
      {...props}
    >
      <path d="M 12 2 C 6.4889971 2 2 6.4889971 2 12 C 2 17.511003 6.4889971 22 12 22 C 17.511003 22 22 17.511003 22 12 C 22 6.4889971 17.511003 2 12 2 z M 12 4 C 16.430123 4 20 7.5698774 20 12 C 20 13.85307 19.369262 15.55056 18.318359 16.904297 L 7.0957031 5.6816406 C 8.4494397 4.6307377 10.14693 4 12 4 z M 5.6816406 7.0957031 L 16.904297 18.318359 C 15.55056 19.369262 13.85307 20 12 20 C 7.5698774 20 4 16.430123 4 12 C 4 10.14693 4.6307377 8.4494397 5.6816406 7.0957031 z"></path>
    </svg>
  </div>
);

export default RejectIcon;
